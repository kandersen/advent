module Advent.TwentySeventeen.One where

import Advent.Library
import Text.Megaparsec
import Data.Char (digitToInt)

main :: IO ()
main = defaultMain "2017.1" (many digitChar) $ \s -> do
  putStrLn s
  print . process . map digitToInt $ s
  print . partTwo . map digitToInt $ s

process :: [Int] -> Int
process [] = 0
process ns@(first:_) = go ns
  where
    go [] = 0
    go [n] = if n == first then n else 0
    go (n:ns'@(m:_)) = (if n == m then n else 0) + go ns'

partTwo :: [Int] -> Int
partTwo digits = go $ zip digits (secondHalf ++ firstHalf)
  where
    go [] = 0
    go ((a,b):ps) | a == b = a + go ps
                  | otherwise = go ps

    (firstHalf, secondHalf) = splitAt midPoint digits
    midPoint = length digits `div` 2