{-# LANGUAGE TemplateHaskell #-}
module Advent.TwentyEighteen.Seven where

import Advent.Library
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State
import Lens.Micro.Platform  

type Node = Char
type Edge = (Node,Node)

succs :: Node -> [Edge] -> [Node]
succs n edges = [m | (n',m) <- edges, n == n' ]

pEdge :: Parser Edge
pEdge = (,) <$ string "Step " <*> upperChar <* 
  string " must be finished before step " <*> upperChar <*
  string " can begin."

data TopoState = TS {
  _marked :: Set Node,
  _unmarked :: Set Node,
  _result :: [Node]
}
makeLenses ''TopoState

type TopoSorter a = State TopoState a

topoOrder :: Set Node -> [Edge] -> [Node]
topoOrder nodes edges = _result $ execState go TS {
  _marked = Set.empty,
  _unmarked = nodes,
  _result = [] }
  where
    go :: TopoSorter ()
    go = do
      mn <- selectUnmarked 
      case mn of
        Nothing -> return ()
        Just n -> visit n >> go

    visit :: Node -> TopoSorter ()
    visit n = do
      nIsMarked <- isMarked n
      unless nIsMarked $ do
        sequence_ . fmap visit $ succs n edges
        marked %= Set.insert n
        result %= (n:)
    
    selectUnmarked :: TopoSorter (Maybe Node)
    selectUnmarked = do
      mr <- Set.maxView <$> use unmarked
      case mr of
        Nothing -> return Nothing
        Just (a, um') -> unmarked .= um' >> return (Just a)

    isMarked :: Node -> TopoSorter Bool
    isMarked n = Set.member n <$> use marked



main :: IO ()
main = defaultMain "2018.7" (pLines pEdge) $ \edges -> do
  let nodes = Set.fromList $ concatMap (\(f, t) -> [f, t]) edges
  print $ topoOrder nodes edges