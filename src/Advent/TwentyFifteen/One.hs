module One where

import Advent

parse :: String -> [Int]
parse = map parse
  where
    parse '(' =  1
    parse ')' = -1
    parse   _ =  0

cumulativeFloors :: [Int] -> [Int]
cumulativeFloors = scanl (+) 0

main :: IO ()
main = defaultMain $ \input -> do
  let steps = parse input
  print $ sum steps
  print $ length . takeWhile (>= 0) . cumulativeFloors $ steps
