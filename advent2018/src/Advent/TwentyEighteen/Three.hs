module Advent.TwentyEighteen.Three where

import Advent.Library

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST
import Data.Array hiding (bounds)
import Control.Arrow (first)

type BoundingBox = (Int, Int, Int, Int)

type Square = (Int, BoundingBox)

bounds :: BoundingBox -> BoundingBox -> BoundingBox
bounds (x1, y1, w1, h1) (x2, y2, w2, h2) = 
    ( x1 `min` x2
    , y1 `min` y2
    , (x1 + w1) `max` (x2 + w2)
    , (y1 + h1) `max` (y2 + h2)
    )

intersects :: BoundingBox -> BoundingBox -> Bool
intersects (x1, y1, w1, h1) (x2, y2, w2, h2) = 
  not $ x1 + w1 - 1 < x2 || x2 + w2 - 1 < x1 || y1 + h1 - 1 < y2 || y2 + h2 - 1 < y1

within :: BoundingBox -> BoundingBox -> Bool
within (ix, iy, iw, ih) (ox, oy, ow, oh) = 
  ox <= ix && ix + iw <= ox + ow &&
  oy <= iy && iy + ih <= oy + oh

data BBTree a = BB BoundingBox (Either [BBTree a] a)

singleton :: BoundingBox -> a -> BBTree a
singleton bb a = BB bb $ Right a

insert :: Square -> BBTree Int -> (BBTree Int, [Int])
insert sq@(sqid,bb1) bb@(BB bb2 t) = 
  if not $ intersects bb1 bb2
  then (BB (bounds bb1 bb2) (Left [bb, singleton bb1 sqid]), [])
  else case t of
    Right tid -> (BB (bounds bb1 bb2) $ Left [singleton bb1 sqid, singleton bb2 tid], [tid])
    Left bbs -> case insertInContainer sq bbs of
      Just _ -> undefined
      Nothing -> undefined

insertInContainer = undefined



pSquare :: Parser Square
pSquare = (\id x y w h -> (id, (x, y, w, h))) <$
  char '#' <*> natural <* string " @ " <*>
  natural <* char ',' <*> natural <* string ": " <*>
  natural <* char 'x' <*> natural

type Cloth s = STArray s (Int,Int) Int

markCell :: Cloth s -> (Int, Int) -> ST s ()
markCell c i = 
  readArray c i >>= writeArray c i . (+1)

markSquare :: Cloth s -> Square -> ST s ()
markSquare c (id, (x, y, w, h)) = 
  forM_ (range ((x, y), (x + w - 1, y + h - 1))) $ 
    markCell c 
        
-- return (overlap, id of non-overlapping square)
countOverlap :: (Int,Int) -> [Square] -> Int
countOverlap dims squares = runST $ do
    cloth <- newArray ((0,0), dims) 0
    forM_ squares (markSquare cloth)
    elems <- getElems cloth
    return $ length . filter (>= 2) $ elems
    
findNonOverlapping :: [Square] -> Square
findNonOverlapping = go
  where
    go [x] = x
    go (x:xs) = go2 (go xs) x xs

    go2 k x []     = x
    go2 k x (y:xs) | snd x `intersects` snd y = k
                   | otherwise = go2 k x xs

findNonOverlapping' :: [Square] -> Square
findNonOverlapping' = go
  where
    go [x] = x
    go (x:xs) = go2 go x xs

    go2 k x []     = x
    go2 k x (y:xs) | snd x `intersects` snd y = k xs
                   | otherwise = go2 (k . (y:)) x xs
    
    go3 = undefined

findNonOverlappingBF :: [Square] -> [Int]
findNonOverlappingBF squares = go squares
  where
    go [] = []
    go ((id1,bb1):xs) = if all (\(id2,bb2) -> id1 == id2 || not (bb1 `intersects` bb2)) squares
      then id1 : go xs
      else go xs

main :: IO ()
main = defaultMain "2018.3" (pLines pSquare) $ \squares -> do
    print $ countOverlap (999,999) squares
    print $ findNonOverlapping squares
    print $ findNonOverlapping' squares
    print $ findNonOverlappingBF squares
