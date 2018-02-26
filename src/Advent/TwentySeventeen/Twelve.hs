module Advent.TwentySeventeen.Twelve where

import Advent.Library
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List ((\\))

type Clause = (Int, [Int])

clause :: Parser (Graph -> Graph)
clause = addEdges <$> natural <* string " <-> " <*> natural `sepBy1` string ", "

type Graph = IntMap [Int]

empty :: Graph
empty = IM.empty

addEdge :: Int -> Int -> Graph -> Graph
addEdge from to = IM.alter g to . IM.alter f from
  where
    f Nothing = Just [to]
    f (Just tos) = Just $ to:tos
    g Nothing = Just [from]
    g (Just froms) = Just $ from:froms

addEdges :: Int -> [Int] -> Graph -> Graph
addEdges from tos = foldl (.) id $ fmap (addEdge from) tos

reachableFrom :: Int -> Graph -> [Int]
reachableFrom source graph = go source id [source] 
  where
    go n k seen = goList (IM.findWithDefault [] n graph) k seen 

    goList [] k seen = k seen
    goList (m:ms) k seen | m `elem` seen = goList ms k seen
                         | otherwise = goList ms (go m k) (m:seen) 

groups :: Graph -> [Int] -> [[Int]]
groups graph = go 
  where
    go [] = []
    go (node:rest) = 
      let component = reachableFrom node graph in
      component : go (rest \\ component)


main :: IO ()
main = defaultMain "2017.12" (pLines clause) $ \clauses -> do
  let graph = foldl (.) id clauses $ empty
  let nodes = IM.keys graph
  print . length . reachableFrom 0 $ graph
  print $ length $groups graph nodes