{-# LANGUAGE TemplateHaskell #-}
module Advent.TwentyEighteen.SevenB where

import Advent.Library
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State
import Lens.Micro.Platform  

import Data.Char (ord)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Node = Char
type Edge = (Node,Node)

succs :: Node -> [Edge] -> [Node]
succs n es = [m | (n',m) <- es, n == n' ]

preds :: Node -> [Edge] -> [Node]
preds n es = [m | (m, n') <- es, n == n' ]

nodes :: [Edge] -> Set Node
nodes = Set.fromList . concatMap (\(f, t) -> [f, t]) 

getRoots :: [Edge] -> Set Node -> Set Node
getRoots es = Set.filter (\n -> null $ preds n es)

pEdge :: Parser Edge
pEdge = (,) <$ string "Step " <*> upperChar <* 
  string " must be finished before step " <*> upperChar <*
  string " can begin."

data TopoState = TS {
  _roots :: Set Node,
  _edges :: [Edge],
  _result :: [Node]
}
makeLenses ''TopoState

type TopoSorter a = State TopoState a

topoOrder :: Set Node -> [Edge] -> [Node]
topoOrder nodes initEdges = _result $ execState go TS {
  _roots = getRoots initEdges nodes,
  _edges = initEdges,
  _result = [] }
  where
    go :: TopoSorter ()
    go = do
      mn <- Set.minView <$> use roots
      case mn of
        Nothing -> return ()
        Just (r,rs') -> do
          roots .= rs'
          visit r
          go

    visit :: Node -> TopoSorter ()
    visit n = do
      result %= flip snoc n
      es <- use edges
      forM_ (succs n es) $ \m -> do
        edges %= filter (/= (n,m))
        mIsRoot <- null . preds m <$> use edges
        when mIsRoot $
          roots %= Set.insert m

data SimState = SS {
  _time :: Int,
  _schedule :: [Node],
  _availableTasks :: [Node],
  _workers :: IntMap (Maybe (Node,Int)) }
makeLenses ''SimState

type Simulator = State SimState

runSim :: [Edge] -> [Node] -> Int -> Int -> Int
runSim edges schedule0 baseCost workerCount = 
  _time . execState go $ SS {
    _time = 0,
    _schedule = schedule0,
    _workers = IntMap.fromList $ zip workerList (repeat Nothing)
  }
  where 
    workerList :: [Int]
    workerList = [1..workerCount]

    go :: Simulator ()
    go = do
      allIdle <- and <$> forM workerList stepWorker
      tick
      unless allIdle go
    
    stepWorker :: Int -> Simulator Bool
    stepWorker w = do
      ws <- (IntMap.! w) <$> use workers
      case ws of
        Nothing -> findWork w          
        Just (task, t) ->
          if ord task - 64 == t
          then findWork w
          else return False
    
    findWork :: Int -> Simulator Bool
    findWork w = do
      mt <- pickTask
      case mt of
        Nothing -> return True
        Just task -> do
          workers %= IntMap.insert w (Just (task, 0))
          return False
    
    pickTask = undefined
    allIdle = undefined
    tick = undefined



main :: IO ()
main = defaultMain "2018.7" (pLines pEdge) $ \edges -> 
  putStrLn $ topoOrder (nodes edges) edges