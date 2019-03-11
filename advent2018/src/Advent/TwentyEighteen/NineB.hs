{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Advent.TwentyEighteen.NineB (main) where

import Advent.Library
import Data.IntMap as IntMap

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Monad.State
import Lens.Micro.Platform
import Control.Monad.ST
import Data.Array.ST

data GameState s = GS {
  _scores :: IntMap Int,
  _turns :: [Int],
  _nextMarble :: Int,
  _highestMarble :: Int,
  _focusedMarble :: Int, 
  _circle :: STArray s Int (Int, Int)
}
makeLenses ''GameState

type Sim s a = StateT (GameState s) (ST s) a

currentPlayer :: Sim s Int
currentPlayer = head <$> use turns

passTurn :: Sim s ()
passTurn = turns %= tail

addScore :: Int -> Sim s ()
addScore m = do
  p <- currentPlayer
  Just s <- use $ scores . at p
  scores . at p ?= s + m

takeTurn :: Int -> Sim s ()
takeTurn m = 
  if m `mod` 23 == 0
  then do
      move (-7)
      m' <- takeMarble
      addScore (m + m')
  else do
      move 2
      placeMarble m

neighbours :: Int -> Sim s (Int, Int)
neighbours m = do
  c <- use circle 
  lift $ readArray c m

writeNeighbours :: Int -> (Int, Int) -> Sim s ()
writeNeighbours m ns = do
  c <- use circle
  lift $ writeArray c m ns

move :: Int -> Sim s ()
move m | m >= 0 = replicateM_ m moveRight
       | m < 0  = replicateM_ (abs m) moveLeft

moveLeft, moveRight :: Sim s ()
moveLeft = do
  (l, _) <- neighbours =<< use focusedMarble
  focusedMarble .= l
moveRight = do
  (_, r) <- neighbours =<< use focusedMarble
  focusedMarble .= r

--  ll l (i) r
--  ll l (m) i r
placeMarble :: Int -> Sim s ()
placeMarble m = do
  i <- use focusedMarble
  (l, r) <- neighbours i
  (ll, _) <- neighbours l
  writeNeighbours l (ll, m)
  writeNeighbours m (l, i)
  writeNeighbours i (m, r)
  focusedMarble .= m
    
-- ll l (i) r rr
-- ll l (r) rr
takeMarble :: Sim s Int
takeMarble = do
  i <- use focusedMarble
  (l, r) <- neighbours i
  (_, rr) <- neighbours r
  (ll, _) <- neighbours l
  writeNeighbours l (ll, r)
  writeNeighbours r (l, rr)
  focusedMarble .= r
  return i

initialize :: Int -> Int -> ST s (GameState s)
initialize players highestMarble = do
    initCircle <- newArray (0, highestMarble) (0,0)
    return GS { 
      _scores = IntMap.fromList $ zip [1..players] [0,0..],
      _turns = cycle [1..players],
      _nextMarble = 1,
      _highestMarble = highestMarble,
      _focusedMarble = 0,
      _circle = initCircle
      }

simulate :: Int -> Int -> Int
simulate ps hm = runST $ do 
  gs' <- initialize ps hm >>= execStateT run
  return $ maximum . IntMap.elems . _scores $ gs'
  where
    run :: StateT (GameState s) (ST s) ()
    run = do
      m <- nextMarble <<%= (+1)
      isDone <- (m >) <$> use highestMarble
      unless isDone $ do
        takeTurn m
        passTurn
        run
            
pProblem :: Parser (Int,Int)
pProblem = (,) <$> natural <* 
  string " players; last marble is worth " <*>
  natural <*
  string " points"

main :: IO ()
main = defaultMain "2018.9" pProblem $ \(players, marbles) -> do
  print $ simulate players marbles
  print $ simulate players (marbles * 100)
  print $ simulate 10 1618