-- |

module Thirteen where
import Advent
import           Data.Map (Map)
import qualified Data.Map as Map

seatAt :: a -> [a] -> [[a]]
seatAt x [] = []
seatAt x (a:bs) = (a:x:bs) : map (a:) (seatAt x bs)

seatings :: [a] -> [[a]]
seatings [] = [[]]
seatings [x] = [[x]]
seatings [x,y] = [[x,y]]
seatings [x,y,z] = [[x,y,z]]
seatings (x:xs) = seatings xs >>= seatAt x

evalSeating :: (a -> a -> Int) -> [a] -> Int
evalSeating pf [] = 0
evalSeating pf xs@(first:_) = go xs
  where
    go [] = 0
    go [last] = pf first last + pf last first
    go (x:y:xs) = pf y x + pf x y + go (y:xs)

parseLine :: String -> Map (String,String) Int -> Map (String, String) Int
parseLine input = case words input of
  [a,_,sign,n,_,_,_,_,_,_,b] ->
    Map.insert pair score
      where
        pair = (a, init b)
        score = read n * case sign of
          "gain" -> 1
          "lose" -> (-1)

parseInput :: String -> Map (String,String) Int
parseInput = foldl (flip ($)) (Map.empty) . map parseLine . lines

getGuests :: String -> [String]
getGuests = nub . map (head . words) . lines

main :: IO ()
main = defaultMain $ \input -> do
  let table = parseInput input
  let guests = "me" : getGuests input
  let pf "me" _ = 0
      pf _ "me" = 0
      pf a b    = table Map.! (a,b)
  print $ maximum . map (evalSeating pf) $ seatings guests
