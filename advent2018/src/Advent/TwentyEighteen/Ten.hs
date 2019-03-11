{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Advent.TwentyEighteen.Ten where

import Advent.Library

type Vec2 = (Int, Int)

instance Num Vec2 where
  (a,b) + (x,y) = (a + x, b + y)
  (a,b) * (x,y) = (a * x, b * y)
  fromInteger n = (fromInteger n, fromInteger n)

pVec2 :: Parser Vec2
pVec2 = (,) <$ space <*> integer <* space <* char ',' <* space <*> integer

pPoint :: Parser (Vec2, Vec2)
pPoint = (,) <$ string "position=<" <*> pVec2 <* string "> velocity=<" <*> pVec2 <* char '>'

bounds :: [Vec2] -> (Vec2,Vec2)
bounds ps = 
  ((minimum (fst <$> ps),minimum (snd <$> ps)),(maximum (fst <$> ps),maximum (snd <$> ps)))

area :: (Vec2, Vec2) -> Int
area ((x1,y1), (x2, y2)) = (x2 - x1) * (y2 - y1)

tick :: [(Vec2, Vec2)] -> [(Vec2, Vec2)]
tick ps = [(p + v, v) | (p, v) <- ps]

monominBy :: Ord a => (b -> a) -> [b] -> b
monominBy _ [x] = x
monominBy f (x:xs@(y:_)) | f x >= f y = monominBy f xs
                         | otherwise  = x

display :: Int -> Int -> [Vec2] -> String
display w h on = go 0 0
    where 
        go x y | x > w           = '\n' : go 0 (y + 1)
               | y > h           = ""
               | (x,y) `elem` on = '#' : go (x+1) y
               | otherwise       = '.' : go (x+1) y

main :: IO ()
main = defaultMain "2018.10" (pLines pPoint) $ \points -> do
    let (smallest,t) = monominBy (area . bounds . fmap fst . fst) $ zip (iterate tick points) [0..]    
    let positions = fst <$> smallest
    let ((x0,y0), _) = bounds positions
    let remapped = (\(x, y) -> (x - x0, y - y0)) <$> positions
    let (_,(dispW,dispH)) = bounds remapped
    putStrLn $ display dispW dispH remapped 
    print t
