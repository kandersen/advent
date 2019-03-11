module Advent.TwentyEighteen.One (main) where

import Advent.Library
import Data.IntSet as Set


findFirstRepeat :: [Int] -> Int
findFirstRepeat = go Set.empty
  where
    go s (n:ns) = 
      if n `Set.member` s
      then n 
      else go (Set.insert n s) ns

main :: IO ()
main = defaultMain "2018.1" (pLines integer) $ \stream -> do
  print . sum $ stream
  print . findFirstRepeat $ scanl (+) 0 (cycle stream)