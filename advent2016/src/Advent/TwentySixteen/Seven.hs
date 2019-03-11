module Advent.TwentySixteen.Seven(main) where

import Data.List
import Control.Arrow
import Control.Monad.State
import Control.Applicative

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Free

breakInput :: String -> ([String], [String])
breakInput = (map snd *** map snd) . partition (isOdd . fst) . zip [1..] . words . map bracketToSpace

isOdd :: Int -> Bool
isOdd = (==1) . (`mod` 2)

bracketToSpace :: Char -> Char
bracketToSpace '[' = ' '
bracketToSpace ']' = ' '
bracketToSpace c = c

-- supportsTLS' :: String -> Bool
-- supportsTLS' = check
-- --  let (outs, ins) = breakInput xs in
-- --  any check outs && not (any check ins)
--   where
    
--     check xs = go xs (tail (tail (tail xs)))
--       where
--         go      _       [] = False
--         go (_:xs) ('[':ys) = not $ go ys (tail (tail (tail ys)))
--         go (_:xs) (']':ys) = not $ go ys (tail (tail (tail ys)))
--         go (x:xs)   (y:ys) =
--           if x == y
--           then case xs of
--             (b:c:_) -> if x /= b && b == c
--                        then True
--                        else go xs ys
--           else go xs ys


supportsTLS :: String -> Bool
supportsTLS = start

start :: String -> Bool
start ('[':xs) = inStart xs
start (x  :xs) = out1 x xs
start       [] = False

out1 _ ('[':xs)             = inStart xs
out1 a ( x :xs) | a == x    = out1 x xs
                | otherwise = out2 a x xs
out1 _       [] = False

out2 _ _ ('[':xs) = inStart xs
out2 a b (x  :xs) | b == x = out3 a b xs
                  | otherwise = out2 b x xs
out2 _ _       [] = False

out3 _ _ ('[':xs) = inStart xs
out3 a b (x : xs) | x == a = outFound xs
                  | x == b = out1 x xs
                  | otherwise = out2 b x xs
out3 _ _       [] = False

outFound :: String -> Bool
outFound ('[':xs) = inFound xs
outFound (_ : xs) = outFound xs
outFound       [] = True

inStart (']': xs) = start xs
inStart (x  : xs) = in1 x xs

in1 :: Char -> String -> Bool
in1 _ (']' : xs) = start xs
in1 a (x : xs) | x == a = in1 x xs
               | otherwise = in2 a x xs

in2 :: Char -> Char -> String -> Bool
in2 _ _ (']' : xs) = start xs
in2 a b (x : xs) | b == x = in3 a b xs
                 | otherwise = in2 b x xs

in3 :: Char -> Char -> String -> Bool
in3 _ _ (']' : xs) = start xs
in3 a b (x : xs) | x == a = False
                 | x == b = in1 x xs
                 | otherwise = in2 b x xs

inFound (']': xs) = outFound xs
inFound (x  : xs) = inFound1 x xs

inFound1 _ (']' : xs) = outFound xs
inFound1 a (x : xs) | x == a = inFound1 x xs
                    | otherwise = inFound2 a x xs

inFound2 _ _ (']' : xs) = outFound xs
inFound2 a b (x : xs) | b == x = inFound3 a b xs
                      | otherwise = inFound2 b x xs

inFound3 _ _ (']' : xs) = outFound xs
inFound3 a b (x : xs) | x == a = False
                      | x == b = inFound1 x xs
                      | otherwise = inFound2 b x xs

data ScannerF a = Found Char Char a
                | Read (Char -> a)
                
type Scanner = Free ScannerF



supportsSSL :: String -> Bool
supportsSSL xs = isJust $ evalStateT (go xs (drop 2 xs)) Set.empty
  where
    go :: String -> String -> StateT (Set (Char,Char,Char)) Maybe ()
    go  _ [] = mzero
    go  _ ('[':xs) = go xs (drop 2 xs)
    go _ (']':xs) = go xs (drop 2 xs)
    go _ (_:'[':xs) = go xs (drop 2 xs)
    go _ (_:']':xs) = go xs (drop 2 xs)
    go (a:xs@(b:c:xs')) (x:ys@(y:z:ys')) = 
      if a == c && b /= c
      then do
        match <- Set.member (b,a,b) <$> get
        unless match $ 
          (modify (Set.insert (a,b,a)) >> go xs' ys') <|> go xs ys
      else go xs ys
    go _ _ = mzero

--         go      _       [] = False
--         go (_:xs) ('[':ys) = not $ go ys (tail (tail (tail ys)))
--         go (_:xs) (']':ys) = not $ go ys (tail (tail (tail ys)))
--         go (x:xs)   (y:ys) =

           
main :: IO ()
main = do
  input <- getContents
  print . length . filter supportsTLS . lines $ input
  print . length . filter supportsSSL . lines $ input
  
