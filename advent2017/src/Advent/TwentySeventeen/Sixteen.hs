{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
module Advent.TwentySeventeen.Sixteen where

import Advent.Library

import Control.Monad.ST
import Data.Array.ST

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

type Arrangement s = STUArray s Int Char

data Command = Spin !Int
             | Exchange !Int !Int
             | Partner !Char !Char
             deriving Show

type Transformation a = Map a a

interpret :: (Ord a) => Transformation a -> (a -> a)
interpret m a = m ! a

compose :: (Ord a) => [a] -> Transformation a -> Transformation a -> Transformation a
compose domain t1 t2 = Map.fromAscList (f <$> domain)
  where
    f c = (c, t2 ! (t1 ! c))

identity :: Ord a => [a] -> Transformation a
identity domain = Map.fromAscList (zip domain domain)

type Substitution = Transformation Char

composeSubs :: Substitution -> Substitution -> Substitution
composeSubs = compose ['a'..'p']

idSub :: Substitution
idSub = identity ['a'..'p']

type Permutation = Map Int Int

composePerms :: Permutation -> Permutation -> Permutation
composePerms = compose [0..15]

idPerm :: Permutation
idPerm = identity [0..15]

toSubstitution :: Command -> Substitution
toSubstitution (Partner a b) = Map.insert a b . Map.insert b a $ idSub
toSubstitution _ = idSub

toPermutation :: Command -> Permutation
toPermutation (Partner _ _) = idPerm
toPermutation (Spin n) = Map.map ((`mod` 16) . (+n)) idPerm
toPermutation (Exchange n m) = Map.insert n m . Map.insert m n $ idPerm

compress :: [Command] -> (Permutation, Substitution)
compress cs = (foldl composePerms idPerm $ toPermutation <$> cs, foldl composeSubs idSub $ toSubstitution <$> cs)

command :: Parser Command
command = Spin <$ char 's' <*> natural
      <|> Exchange <$ char 'x' <*> natural <* char '/' <*> natural
      <|> Partner <$ char 'p' <*> letterChar <* char '/' <*> letterChar

perform :: Command -> Arrangement s -> ST s (Arrangement s)
perform (Spin n) arr = mapIndices (0,15) ((`mod` 16) . (flip (-) n)) arr
perform (Exchange n m) arr = do
  tmp <- readArray arr n
  readArray arr m >>= writeArray arr n
  writeArray arr m tmp
  return arr
perform (Partner a b) arr = mapArray (\x -> if x == a then b else if x == b then a else x) arr

performDance :: [Command] -> Int -> String -> String
performDance dance count start = runST $ do
  arr <- newListArray (0, 15) start
  getElems =<< go count arr dance
  where
    go 1 arr [] = return arr
    go n arr [] = go (n - 1) arr dance
    go n arr (c:cmds) = do 
      arr' <- perform c arr
      go n arr' cmds

performDance' :: [Command] -> String -> String
performDance' dance start = fmap (interpret sub) . Map.elems . Map.mapKeys (interpret perm) $ Map.fromAscList (zip [0..] start)
  where
    (perm, sub) = compress dance

moveSpins :: [Command] -> [Command]
moveSpins [] = []
moveSpins [x] = [x]
moveSpins (Spin n : Spin m : xs) = moveSpins $ (Spin $ n + m) : xs
moveSpins (Spin n : Exchange m o : xs) = Exchange ((m - n) `mod` 16) ((o - n) `mod` 16) : (moveSpins $ Spin n : xs)
moveSpins (p : xs) = p : moveSpins xs

optimize :: [Command] -> [Command]
optimize = moveSpins

performDance'' :: [Command] -> Int -> String -> String
performDance'' dance times start = lookForCycle 0 times start
  where
    stepState = performDance' dance 

    lookForCycle _ 1 state = performDance' dance start
    lookForCycle n times state = 
      let next = stepState state in
      if next == start
        then foundCycle (n + 1) (times - 1) next
        else lookForCycle (n + 1) (times - 1) next

    foundCycle n times state = finish (times `mod` n) state

    finish 0 state = state
    finish n state = finish (n - 1) $ stepState state

main :: IO ()
main = defaultMain "2017.16" (command `sepBy` char ',') $ \prog -> do
  print $ length prog
  print $ length . optimize $ prog
  putStrLn $ performDance prog 1 ['a'..'p']
  putStrLn $ performDance'' (optimize prog) 1 ['a'..'p']
  putStrLn $ performDance'' (optimize prog) 1000000000 ['a'..'p']

