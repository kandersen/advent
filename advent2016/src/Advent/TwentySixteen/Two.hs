{-# LANGUAGE DeriveFunctor #-}
module Advent.TwentySixteen.Two (main) where

import Data.Char (intToDigit)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Free

import Control.Monad.Trans.State

--------------------------------------------------------------------------------

data Dir = U | D | L | R

data CommandF a = Go Dir a
                | Stop a
                deriving (Functor)

type Command = Free CommandF

goUp, goDown, goLeft, goRight :: Command ()
goUp = liftF $ Go U ()
goDown = liftF $ Go D ()
goLeft = liftF $ Go L ()
goRight = liftF $ Go R ()

stop :: Command ()
stop = liftF $ Stop ()

parseCommand :: Char -> Command ()
parseCommand 'U' = goUp
parseCommand 'D' = goDown
parseCommand 'L' = goLeft
parseCommand 'R' = goRight

parseInput :: IO (Command ())
parseInput =  mapM_ ((>> stop) . mapM_ parseCommand) . lines <$> getContents

--------------------------------------------------------------------------------

type Pos = (Int, Int)

type Keypad = Map Pos Char

move :: Keypad -> Dir -> Pos -> Pos
move kp d old@(x,y) =
  if Map.member new kp then new else old
  where
    new = case d of
      U -> (x, y - 1)
      D -> (x, y + 1)
      L -> (x - 1, y)
      R -> (x + 1, y)

digitAt :: Keypad -> Pos -> Char
digitAt kp p = case Map.lookup p kp of
  Nothing -> error $ "-- digitAt: " ++ show p ++ " is out of bounds"
  Just c -> c

keypad :: Keypad
keypad = Map.fromList
  [ ((c,r), intToDigit $ r * 3 + (c + 1)) | r <- [0..2], c <- [0..2] ]

realKeypad :: Keypad
realKeypad = Map.fromList [
  ((0,2), '5'),
  ((1,1), '2'),
  ((1,2), '6'),
  ((1,3), 'A'),
  ((2,0), '1'),
  ((2,1), '3'),
  ((2,2), '7'),
  ((2,3), 'B'),
  ((2,4), 'D'),
  ((3,1), '4'),
  ((3,2), '8'),
  ((3,3), 'C'),
  ((4,2), '9')
  ]

--------------------------------------------------------------------------------

findCode :: Keypad -> Command () -> String
findCode kp c = evalState (interpret c) (1,1)
  where
    interpret :: Command () -> State (Int, Int) String
    interpret (Pure ()) = return ""
    interpret (Free (Stop a)) =
      (:) <$> (digitAt kp <$> get) <*> interpret a
    interpret (Free (Go d a)) = modify (move kp d) >> interpret a

--------------------------------------------------------------------------------

main :: IO ()
main = do
  command <- parseInput
  putStrLn $ "You thought the code was " ++ findCode keypad command
  putStrLn $ "The real code is " ++ findCode realKeypad command
