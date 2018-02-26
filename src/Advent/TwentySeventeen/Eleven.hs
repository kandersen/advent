module Advent.TwentySeventeen.Eleven where

import Advent.Library

type Point = (Int, Int)

origin :: Point
origin = (0,0)

parseStep :: Parser (Point -> Point)
parseStep = try (moveSE <$ string "se")
        <|> try (moveSW <$ string "sw")
        <|> moveS <$ string "s"
        <|> try (moveNW <$ string "nw")
        <|> try (moveNE <$ string "ne")
        <|> moveN <$ string "n"

moveN, moveS, moveNW, moveSE, moveSW, moveNE :: Point -> Point
moveN (x, y) = (x, y - 1)
moveS (x, y) = (x, y + 1)
moveNW (x, y) = (x - 1, y)
moveSE (x, y) = (x + 1, y)
moveSW (x, y) = (x - 1, y + 1)
moveNE (x, y) = (x + 1, y - 1)

axialToCube :: Point -> (Int, Int, Int)
axialToCube (x, y) = (x, y, (-x) - y)

cubeMagnitude :: (Int, Int, Int) -> Int
cubeMagnitude (x, y, z) = (abs x + abs y + abs z) `div` 2

magnitude :: Point -> Int
magnitude = cubeMagnitude . axialToCube

main :: IO ()
main = defaultMain "2017.11" (parseStep `sepBy` char ',') $ \steps -> do
  print . magnitude . ($ origin) . foldl (.) id $ steps
  print . maximum . fmap magnitude $ evolve steps origin
