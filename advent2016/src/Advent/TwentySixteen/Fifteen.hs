module Advent.TwentySixteen.Fifteen(main) where

import Advent.Library
import Data.Maybe (fromJust)
import Data.List (find)

data Disc = Disc Int Int
          deriving Show

pDisc :: Parser Disc
pDisc = Disc <$ string "Disc #" <* natural <* string " has "
            <*> natural <* string " positions; at time=0, it is at position "
            <*> natural <* char '.'

example :: [Disc]
example = [Disc 5 4, Disc 2 1]

passableAt :: [Disc] -> Int -> Bool
passableAt [] _ = True
passableAt (Disc p i:ds) t = (i + t + 1) `mod` p == 0 && passableAt ds (t + 1)
  
solve :: [Disc] -> Int
solve ds = fromJust . find (passableAt ds) $ [0..]

main :: IO ()
main = defaultMain "2016.15" (pLines pDisc) $ \discs -> do
  print $ solve discs
  print $ solve (discs ++ [Disc 11 0])

