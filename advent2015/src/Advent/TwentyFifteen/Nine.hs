module Advent.TwentyFifteen.Nine where

import Advent.Library
import Data.Map (Map, (!))
import Data.List (nub, permutations, minimum, maximum)
import Data.Ord (comparing)
import qualified Data.Map as Map


type City = String

pCity :: Parser City
pCity = some letterChar

type Graph = Map (City, City) Int

pEntry :: Parser (Graph -> Graph)
pEntry = (\a b n -> Map.insert (a, b) n . Map.insert (b, a) n) <$> 
  pCity <* string " to " <*> pCity <* string " = " <*> natural

circuits :: [a] -> [[a]]
circuits = permutations

circuitLength :: Graph -> [City] -> Int
circuitLength graph = go
  where
    go [] = 0
    go [x] = 0
    go (a:xs@(b:_)) = graph ! (a, b) + go xs

main :: IO ()
main = defaultMain "2015.9" (fromBuilders Map.empty <$> pLines pEntry) $ \graph -> do
  let cities = nub $ fst <$> Map.keys graph
  let lengths = circuitLength graph <$> circuits cities
  print $ minimum lengths
  print $ maximum lengths