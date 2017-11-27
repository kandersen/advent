module Fourteen where
import Advent
import Data.List
type Reindeer = (Int, Int, Int, Either Int Int, Int, Int)

parseLine :: String -> Reindeer
parseLine l = case words l of
  [_,_,_,speed,_,_,dur,_,_,_,_,_,_,rest,_] ->
    (read speed, read dur, read rest, Right 0, 0, 0)

parseInput :: String -> [Reindeer]
parseInput = map parseLine . lines

stepReindeer :: Reindeer -> Reindeer
stepReindeer (speed, dur, rest, Right 0, dist, pts) =
  (speed, dur, rest, Left (dur - 1), dist + speed, pts)
stepReindeer (speed, dur, rest, Right n, dist, pts) =
  (speed, dur, rest, Right (n - 1), dist, pts)
stepReindeer (speed, dur, rest, Left 0, dist, pts) =
  (speed, dur, rest, Right (rest - 1), dist, pts)
stepReindeer (speed, dur, rest, Left n, dist, pts) =
  (speed, dur, rest, Left (n - 1), dist + speed, pts)

distanceCovered :: Reindeer -> Int
distanceCovered (_,_,_,_,d,_) = d

points (_, _, _, _, _, pts) = pts

step :: [Reindeer] -> [Reindeer]
step rs = case sortBy (compare `on` distanceCovered) . map stepReindeer $ rs of
  [] -> []
  (r:rs) -> award r : rs
    where
      award (speed, dur, rest, state, dist, pts) =
        (speed, dur, rest, state, dist, pts + 1)

main :: IO ()
main = defaultMain $ \input -> do
  let init = parseInput input
  let final = (iterate step init) !! 2503
  print . maximum $ map points final
