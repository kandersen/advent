module Advent.TwentyEighteen.Six where

import Advent.Library

type Point = (Int, Int)

pPoint :: Parser Point
pPoint = (,) <$> natural <* string ", " <*> natural

bounds :: [Point] -> ((Int, Int), (Int, Int))
bounds ps = ( (minimum $ fst <$> ps
            ,  maximum $ fst <$> ps)
            , (minimum $ snd <$> ps
            ,  maximum $ snd <$> ps) )



largestFiniteArea :: [Point] -> Int
largestFiniteArea points = go
  where
    go = undefined

main :: IO ()
main = defaultMain "2018.6" (pLines pPoint) $ \points -> do
  print points
  print $ bounds points
