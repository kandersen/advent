module Advent.TwentySeventeen.Fifteen where

import Advent.Library
import Data.Bits

parseInput :: Parser (Int, Int)
parseInput = (,) <$ string "Generator A starts with " <*> natural <* eol
                 <* string "Generator B starts with " <*> natural <* eol

generator :: Int -> Int -> Int -> [Int]
generator remainder factor = go
  where
    go prev = 
      let next = (prev * factor) `rem` 2147483647 in
      if next `rem` remainder == 0
        then next : go next
        else go next

judge :: Int -> Int -> Bool
judge a b = (a .&. 0xFFFF) == (b .&. 0xFFFF)

main :: IO ()
main = defaultMain "2017.15" parseInput $ \(seedA, seedB) -> do
  print . length . filter id . take 40000000 $ zipWith judge (generator 1 16807 seedA) (generator 1 48271 seedB)
  print . length . filter id . take 5000000 $ zipWith judge (generator 4 16807 seedA) (generator 8 48271 seedB)