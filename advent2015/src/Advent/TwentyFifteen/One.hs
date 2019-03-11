module Advent.TwentyFifteen.One where

import Advent.Library

cumulativeFloors :: [Int] -> [Int]
cumulativeFloors = scanl (+) 0

pStep :: Parser Int
pStep = 1 <$ char '('
    <|> (-1) <$ char ')'

main :: IO ()
main = defaultMain "2015.1" (many pStep) $ \steps -> do
  print $ sum steps
  print $ length . takeWhile (>= 0) . cumulativeFloors $ steps
