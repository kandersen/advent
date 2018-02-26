{-# LANGUAGE BangPatterns #-}
module Advent.TwentySeventeen.Two where

import Advent.Library

parseRow :: Parser [Int]
parseRow = natural `sepBy` tab

rowSum :: [Int] -> Int
rowSum [] = error $ "Assuming row non-empty" 
rowSum (n:ns) = go n n ns
  where
    go !smallest !biggest [] = biggest - smallest
    go !smallest !biggest (n:ns) = go (smallest `min` n) (biggest `max` n) ns

rowDivisor :: [Int] -> Int
rowDivisor = go 
  where
    go [_] = error $ "Assumed there is a solution - found list of less than two elemens"
    go (n:ns) = look n ns (go ns)

    look n (m:ns) fk | n `rem` m == 0 = n `div` m
                     | m `rem` n == 0 = m `div` n
                     | otherwise      = look n ns fk
    look _ [] fk = fk

main :: IO ()
main = defaultMain "2017.2" (pLines parseRow) $ \input -> do
  print . sum . fmap rowSum $ input
  print . sum . fmap rowDivisor $ input