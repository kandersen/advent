module Three where

import           Advent
import           Control.Monad.State
import           Data.Set (Set)
import qualified Data.Set as Set

type Position = (Int, Int)
type History = Set Position

data Direction = N | S | E | W

parseDirection :: Char -> Direction
parseDirection '^' = N
parseDirection 'v' = S
parseDirection '>' = E
parseDirection '<' = W

step :: Direction -> Position -> Position
step N (x, y) = (x, y + 1)
step S (x, y) = (x, y - 1)
step E (x, y) = (x + 1, y)
step W (x, y) = (x - 1, y)

travel :: Direction -> State (Position, History) ()
travel dir = do
  (pos, history) <- get
  let next = step dir pos
  put (next, Set.insert next history)

split :: [a] -> ([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:as) = let (xs, ys) = split as
                 in (x:xs, y:ys)

housesVisited :: [Direction] -> Set Position
housesVisited steps =
  snd $ execState (sequence_ (map travel steps)) (init, Set.fromList [init])
  where
    init = (0,0)

main :: IO ()
main = defaultMain $ \input -> do
  let steps = map parseDirection input
  print . Set.size . housesVisited $ steps
  let (santa,robosanta) = split steps
  print . Set.size $  housesVisited santa `Set.union` housesVisited robosanta
