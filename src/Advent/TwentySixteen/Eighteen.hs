module Eighteen(main) where

import Data.Array
--import qualified Data.Array as Array

buildMap :: [Bool] -> Int -> Array (Int,Int) Bool
buildMap firstRow rows = result
  where
    result :: Array (Int,Int) Bool
    result = array ((1,1), (length firstRow, rows)) $ zip (range ((1,1),(length firstRow, 1))) firstRow ++ els

    parents :: (Int,Int) -> [(Int,Int)]
    parents (x,y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]

    readRes :: (Int, Int) -> Bool
    readRes p@(x,y) = if 1 <= x && x <= length firstRow
                      then result ! p
                      else False

    isTrap :: (Int,Int) -> Bool
    isTrap p = case map readRes (parents p) of
      [True, True, False] -> True
      [False, True, True] -> True
      [True, False, False] -> True
      [False, False, True] -> True
      _ -> False
      

    els :: [((Int,Int), Bool)]
    els = [ (i, isTrap i) | i <- range ((1,2),(length firstRow, rows)) ]

parseRow :: String -> [Bool]
parseRow = map (=='^')

example :: String
example = ".^^.^.^^^^"

main :: IO ()
main = do
  firstRow <- parseRow <$> getLine
  print . length . filter not . elems $ buildMap firstRow 40
  print . length . filter not . elems $ buildMap firstRow 400000
