{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Advent.TwentyEighteen.Fourteen where

import Advent.Library
import Lens.Micro.Platform
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.ST
import Data.Array.ST
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set


data GameState s = GS {
  _scoreboard :: STArray s Int Int,
  _recipeCount :: Int,
  _firstElf :: Int,
  _secondElf :: Int,
  _candidatePos :: Set Int,
  _target :: [Int]
}
makeLenses ''GameState

initialState :: [Int] -> Int -> ST s (GameState s)
initialState targ bound = do
  sb <- newArray (0, bound + 11) 0
  writeArray sb 0 3
  writeArray sb 1 7
  return GS { 
    _scoreboard = sb,
    _recipeCount = 2,
    _firstElf = 0,
    _secondElf = 1,
    _candidatePos = Set.empty,
    _target = targ
  }

type Runner s a = StateT (GameState s) (ST s) a

addNew :: Int -> Runner s ()
addNew n = sequence_ $ go <$> show n
  where
    go :: Char -> Runner s ()
    go c = do
      let d = read [c]
      c <- recipeCount <<%= (+1)
      sb <- use scoreboard
      lift $ writeArray sb c d
      targetStart <- head <$> use target
      when (d == targetStart) $
        candidatePos %= Set.insert c

currentNumber :: Lens' (GameState s) Int -> Runner s Int
currentNumber elf = do
  i <- use elf
  sb <- use scoreboard
  lift $ readArray sb i
  
advance :: Lens' (GameState s) Int -> Int -> Runner s ()
advance elf n = do
  m <- use recipeCount
  elf %= (`mod` m) . (+ n)

step :: Runner s ()
step = do
  n <- currentNumber firstElf
  m <- currentNumber secondElf
  addNew $ n + m
  advance firstElf (n + 1)
  advance secondElf (m + 1)

readRange :: (Ix i) => (i, i) -> STArray s i a -> ST s [a]
readRange (l,u) s = sequence $ readArray s <$> range (l, u)

recipesAfter :: Int -> [Int]
recipesAfter n = runST $ do
  conf <- initialState [0] n
  conf' <- execStateT go conf
  readRange (n, n + 9) $ conf' ^. scoreboard
  where
    go = do
      c <- use recipeCount
      when (c < n + 10) $
        step >> go

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (m:ms) = case m of
  Nothing -> firstJust ms
  Just a -> Just a

checkTargetCandidates :: Runner s (Maybe Int)
checkTargetCandidates = do
  t <- use target
  cs <- Set.toList <$> use candidatePos
  firstJust <$> forM cs (checkTarget t) 
  where
    checkTarget :: [Int] -> Int -> Runner s (Maybe Int)
    checkTarget t c = do
      let l = length t
      fits <- (> c + l - 1) <$> use recipeCount
      if fits
        then do
           sb <- use scoreboard
           ns <- lift $ readRange (c, c + l - 1) sb
           if t == ns
            then return (Just c)
            else do
              candidatePos %= Set.delete c
              return Nothing
        else return Nothing
           
searchForInput :: [Int] -> Int
searchForInput target = runST $ do
  conf <- initialState target 100000000
  evalStateT go conf
  where
    go = do
      step 
      maybe go return =<< checkTargetCandidates
  

main :: IO ()
main = defaultMain "2018.14" natural $ \input -> do
  putStrLn . concatMap show $ recipesAfter 9
  putStrLn . concatMap show $ recipesAfter input
  print $ searchForInput [5, 1, 5, 8, 9]
  print $ searchForInput (fmap (read . (:[])) . show $ input)