{-# LANGUAGE DeriveFunctor #-}
module Advent.TwentySixteen.One(main) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either

import Control.Monad.Free

import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad 
import Control.Monad.Identity

import Lens.Micro
import Lens.Micro.Platform

--------------------------------------------------------------------------------

data Direction = R | L
                deriving (Show, Read)

data SantaCommandF a = Turn Direction a
                     | Go Int a
                     deriving (Functor)

type SantaCommand = Free SantaCommandF

turn :: Direction -> SantaCommand ()
turn d = liftF $ Turn d ()

go :: Int -> SantaCommand ()
go n = liftF $ Go n ()

parseInstruction :: String -> SantaCommand ()
parseInstruction (dir:dist) = do
  turn $ read [dir]
  go $ read dist

parseInput :: IO (SantaCommand ())
parseInput = mapM_ parseInstruction . words . map commaToSpace <$> getLine

commaToSpace :: Char -> Char 
commaToSpace ',' = ' '
commaToSpace c = c

--------------------------------------------------------------------------------

type Pos = (Int,Int)

data Heading = North | East | South | West
             deriving (Enum)

applyTurn :: Direction -> Heading -> Heading
applyTurn R  West = North
applyTurn R     h = succ h
applyTurn L North = West
applyTurn L     h = pred h

move :: Heading -> Int -> Pos -> Pos
move North n (x, y) = (x, y + n)
move East  n (x, y) = (x + n, y)
move South n (x, y) = (x, y - n)
move West  n (x, y) = (x - n, y)

--------------------------------------------------------------------------------

findFinalPosition :: SantaCommand () -> (Int,Int)
findFinalPosition sc = fst $ execState (interpret sc) ((0,0), North)
  where
    interpret :: SantaCommand a -> State (Pos, Heading) a
    interpret (Pure a) = return a
    interpret (Free (Turn d a)) = do
      _2 %= applyTurn d
      interpret a
    interpret (Free (Go n a)) = do
      h <- use _2
      _1 %= move h n
      interpret a

findFirstPositionVisitedTwice :: SantaCommand () -> (Int, Int)
findFirstPositionVisitedTwice sc =
  either id (^. _1) $ execStateT (interpret sc) ((0,0), North, Set.empty)
  where
    interpret :: SantaCommand a -> StateT (Pos, Heading, Set Pos) (Either Pos) a
    interpret (Pure a) = return a
    interpret (Free (Turn d a)) = do
      _2 %= applyTurn d
      interpret a
    interpret (Free (Go n a)) = do
      replicateM_ n $ do
        h <- use _2
        _1 %= move h 1
        newPos <- use _1
        beenBefore <- Set.member newPos <$> use _3
        if beenBefore
          then lift $ Left newPos
          else _3 %= Set.insert newPos
      interpret a

main :: IO ()
main = do
  cmd <- parseInput
  let (x,y) = findFinalPosition cmd
  putStrLn $ "Distance to final position: " ++ (show . abs $ x + y)
  let (x,y) = findFirstPositionVisitedTwice cmd
  putStrLn $ "Distance to first position visited twice: " ++ (show . abs $ x + y)
