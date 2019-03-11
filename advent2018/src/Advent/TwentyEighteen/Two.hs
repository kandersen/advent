module Advent.TwentyEighteen.Two where

import Advent.Library
import Data.Map (Map, (!))
import Data.Map.Merge.Strict (merge, traverseMissing, zipWithMatched)
import qualified Data.Map as Map
import Control.Monad

tally :: String -> Map Char Int
tally = foldr (Map.alter insertOrInc) Map.empty

insertOrInc :: Maybe Int -> Maybe Int
insertOrInc Nothing = Just 1
insertOrInc (Just n) = Just (n + 1)

distance :: Map Char Int -> Map Char Int -> Int
distance m1 m2 = (`div` 2) . sum . Map.elems $
  merge notInTheOther notInTheOther whenMatch m1 m2
    where
      whenMatch = zipWithMatched $ \_ x y ->
          abs $ y - x
      notInTheOther = traverseMissing $ \_ x ->
          pure x
    
findBoxes :: [String] -> (String, String)
findBoxes ids = head [ (a, b) | a <- ids, b <- ids, oneEditBetween a b ]

oneEditBetween :: String -> String -> Bool
oneEditBetween a b = (==1) . length . filter id $ zipWith (/=) a b

findThoseInCommon :: String -> String -> String
findThoseInCommon a b = go $ zip a b
  where
    go [] = []
    go ((x, y):xs) | x == y    = x : go xs
                   | otherwise = go xs

main :: IO ()
main = defaultMain "2018.2" (pLines $ some letterChar) $ \inputs -> do
  let sigs = tally <$> inputs
  let twos = length $ filter ((2 `elem`) . Map.elems) sigs
  let threes = length $ filter ((3 `elem`) . Map.elems) sigs
  print $ twos * threes
  let (i, j) = findBoxes inputs
  putStrLn $ findThoseInCommon i j
