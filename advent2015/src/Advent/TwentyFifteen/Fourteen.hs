{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Advent.TwentyFifteen.Fourteen where

import Advent.Library

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.State

import Data.List
import Data.Function
import Data.Ord
import Control.Arrow (second)
import Lens.Micro.Platform

data ReindeerStats = ReindeerStats {
  _speed :: Int,
  _endurance :: Int,
  _rest :: Int
  }
  deriving Show
makeLenses ''ReindeerStats

data ReindeerState = ReindeerState {
  _status :: Either Int Int,
  _pos :: Int,
  _score :: Int
  }
  deriving Show
makeLenses ''ReindeerState

initState :: ReindeerStats -> ReindeerState
initState stats = ReindeerState (Right $ _endurance stats) 0 0
  
pStats :: Parser (String, ReindeerStats)
pStats = (\n sp end rest -> (n, ReindeerStats sp end rest)) <$>
  some letterChar <* string " can fly " <*> natural <* string " km/s for " <*> natural
  <* string " seconds, but then must rest for " <*> natural <* string " seconds."

type Simulator a = ReaderT (Map String ReindeerStats) (State (Map String ReindeerState)) a

stepReindeer :: ReindeerStats -> ReindeerState -> ReindeerState
stepReindeer stats s = 
  case s ^. status of
    Right 1 -> 
      s & status .~ Left (stats ^. rest - 1)
        & pos %~ (+ (stats ^. speed))
    Right n -> 
      s & status .~ Right (n - 1)
        & pos %~ (+ (stats ^. speed))
    Left 0 -> 
      s & status .~ Right (stats ^. endurance)
    Left n -> 
      s & status .~ Left (n - 1)

award :: String -> Simulator ()
award reindeer = 
  modify (Map.update (Just . (& score %~ (+1))) reindeer)

findWinner :: [String] -> Simulator String
findWinner names = do
  positions <- forM names $ \name -> do
    s <- flip (Map.!) name <$> get
    return (name, s ^. pos)
  return $ fst . maximumBy (comparing snd) $ positions

stepSimulation :: Simulator ()
stepSimulation = do
  names <- Map.keys <$> get
  forM_ names $ \name -> do
    stats <- flip (Map.!) name <$> ask
    modify (Map.update (Just . stepReindeer stats) name)
  findWinner names >>= award
  

runSimulator :: Map String ReindeerStats -> Map String ReindeerState -> Int -> Map String ReindeerState
runSimulator stats state n = execState (runReaderT (replicateM n stepSimulation) stats) state

main :: IO ()
main = defaultMain "2015.14" (pLines pStats) $ \rawStats -> do
  let statMap = Map.fromList rawStats
  let stateMap = initState <$> statMap
  let finalState = runSimulator statMap stateMap 2503
  print . maximum $ map _pos $ Map.elems finalState
  print . maximum $ map _score $ Map.elems finalState
