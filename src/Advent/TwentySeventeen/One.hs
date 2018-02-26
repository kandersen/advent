{-# LANGUAGE BangPatterns #-}
module Advent.TwentySeventeen.One where

import Advent.Library
import Data.Char (digitToInt)

main :: IO ()
main = defaultMain "2017.1" (many digitChar) $ \s -> do
  putStrLn s
  print . process . map digitToInt $ s
  print . partTwo . map digitToInt $ s
  print . partTwo' . map digitToInt $ s

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

partTwo' :: [Int] -> Int
partTwo' xs = findEnd xs xs [] []
  where
    findEnd _ [] halfway tail = go (halfway) tail 0
    findEnd (x:xs) (a:b:cs) halfway tail = findEnd xs cs (x:halfway) (b:a:tail)
    findEnd _ _ _ _ = error "Shouldn't happen"    
    go (x:xs) (y:ys) !ans = go xs ys (if x == y then ans + x + y else ans)
    go [] _ !ans = ans
{-
partTwo'' :: [Int] -> Int
partTwo'' xs = findEnd xs xs (\ys zs -> 0)
  findEnd _ [] k = k [] []
  findEnd (x:xs) (a:b:cs) k = findEnd xs cs (\)
-}