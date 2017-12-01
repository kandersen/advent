module Twenty(main) where

import Advent
import Text.Megaparsec

import Data.List (sort)

type Interval = (Int,Int)

firstFree :: [Interval] -> Int
firstFree = go 0 0 . sort
  where
    go _ n         [] = n
    go g n ((l,h):is) | h <= g    = go g n is
                      | n < l     = n
                      | otherwise = go (max g h) (h + 1) is

allowed :: [Interval] -> Int
allowed = go 0 0 . sort
  where
    go g acc [] = acc + ((2^32 - 1 - g) `max` 0)
    go g acc ((l,h):is) | h <= g     = go g acc is
                        | l <= g     = go (h + 1) acc is
                        | otherwise  = go (h + 1) (acc + (l - g)) is

parseInterval :: Parser Interval
parseInterval = (\a b -> (min a b, max a b)) <$> integer <* char '-' <*> integer

main :: IO ()
main = do
  is <- parseLines  parseInterval <$> getContents
  print . firstFree $ is
  print . allowed $ is
