{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Advent.TwentyEighteen.Nine where

import Advent.Library
import Data.IntMap as IntMap

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Monad.State
import Lens.Micro.Platform

pProblem :: Parser (Int,Int)
pProblem = (,) <$> natural <* 
  string " players; last marble is worth " <*>
  natural <*
  string " points"

type Circle = (Int, Seq Int)

move :: Int -> Circle -> Circle
move n (i, c) = ((i + n) `mod` Seq.length c , c)

placeMarble :: Int -> Circle -> Circle
placeMarble m (i, c) = (i, Seq.insertAt i m c)

removeMarble :: Circle -> Circle
removeMarble (i, c) = (i, Seq.deleteAt i c)

currentMarble :: Circle -> Int
currentMarble (i, c) = Seq.index c i

data GameState = GS {
  _scores :: IntMap Int,
  _turns :: [Int],
  _nextMarble :: Int,
  _highestMarble :: Int,
  _circle :: (Int, Seq Int)
}
makeLenses ''GameState

displayGS :: GameState -> String
displayGS GS {..} = concat [
    "[", show . head $ _turns, "] ",
    Seq.foldMapWithIndex (\n m -> if n == selected then "(" ++ show m ++ ")" else " " ++ show m ++ " ") marbles
  ]
  where
    (selected, marbles) = _circle

initialize :: Int -> Int -> GameState 
initialize players highestMarble = 
    let initScores = IntMap.fromList $ zip [1..players] [0,0..]
        initCircle = (0, Seq.singleton 0) in
    GS initScores (cycle [1..players]) 1 highestMarble initCircle

simulate :: Int -> Int -> IO Int
simulate ps hm = maximum . IntMap.elems . _scores <$> 
  execStateT run (initialize ps hm)
  where
    run :: StateT GameState IO ()
    run = do
      m <- nextMarble <<%= (+1)
      isDone <- (m >) <$> use highestMarble
      unless isDone $ do
        takeTurn m
        passTurn
        run

    currentPlayer :: StateT GameState IO Int
    currentPlayer = head <$> use turns

    passTurn :: StateT GameState IO ()
    passTurn = turns %= tail

    addScore :: Int -> Int -> StateT GameState IO ()
    addScore p m = do
      Just s <- use $ scores . at p
      scores . at p ?= s + m

    takeTurn :: Int -> StateT GameState IO ()
    takeTurn m = 
        if m `mod` 23 == 0
        then do
            p <- currentPlayer
            circle %= move (-7) 
            m' <- currentMarble <$> use circle
            circle %= removeMarble
            addScore p (m + m')
        else do
            circle %= move 2 
            circle %= placeMarble m
      
nextIndex :: Int -> Int -> Int
nextIndex current count = current + 2 `mod` count

main :: IO ()
main = defaultMain "2018.9" pProblem $ \(players, marbles) -> do
  print =<< simulate players marbles
  print =<< simulate players (marbles * 100)

-- n = n - (2 * (n `div` 23))