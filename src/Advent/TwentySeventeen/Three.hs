{-# LANGUAGE BangPatterns #-}
module Advent.TwentySeventeen.Three where

import Advent.Library
import Data.Map (Map)
import Control.Monad.State
import qualified Data.Map as Map

cells :: [(Integer,Integer)]
cells = goRight 1 1 0 0 
  where
    goRight !d !0 !x !y = goUp d d x y
    goRight !d !n !x !y = (x, y) : goRight d (n - 1) (x + 1) y
    goUp    !d !0 !x !y = goLeft (d + 1) (d + 1) x y
    goUp    !d !n !x !y = (x, y) : goUp d (n - 1) x (y + 1)
    goLeft  !d !0 !x !y = goDown d d x y
    goLeft  !d !n !x !y = (x, y) : goLeft d (n - 1) (x - 1) y
    goDown  !d !0 !x !y = goRight (d + 1) (d + 1) x y
    goDown  !d !n !x !y = (x, y) : goDown d (n - 1) x (y - 1)

cells' :: [(Integer,Integer)]
cells' = goRight 1 1 0 0 
  where
    goRight !d !1 !x !y = (x, y) : goUp d d (x + 1) y
    goRight !d !n !x !y = (x, y) : goRight d (n - 1) (x + 1) y
    goUp    !d !1 !x !y = (x, y) : goLeft (d + 1) (d + 1) x (y + 1)
    goUp    !d !n !x !y = (x, y) : goUp d (n - 1) x (y + 1)
    goLeft  !d !1 !x !y = (x, y) : goDown d d (x - 1) y
    goLeft  !d !n !x !y = (x, y) : goLeft d (n - 1) (x - 1) y
    goDown  !d !1 !x !y = (x, y) : goRight (d + 1) (d + 1) x (y - 1)
    goDown  !d !n !x !y = (x, y) : goDown d (n - 1) x (y - 1)

type Point = (Integer, Integer)

cells'' :: [(Integer,Integer, Integer)]
cells'' = (:) (0,0,1) $ flip evalState (Map.fromList [((0,0), 1), ((1,0), 1)]) $ goUp 1 1 1 0 
  where
    lookup :: Point -> State (Map Point Integer) Integer
    lookup p = Map.findWithDefault 0 p <$> get

    goRight :: Int -> Int -> Integer -> Integer -> State (Map Point Integer) [(Integer, Integer, Integer)]
    goRight !d !1 !x !y = do 
      left <- lookup (x - 1, y)
      leftUp <- lookup (x -1, y + 1)
      rest <- goUp d d (x + 1) y
      return $ (x, y, left + leftUp) : rest
    goRight !d !n !x !y = do
      left <- lookup (x - 1, y)
      leftUp <- lookup (x - 1, y + 1)
      up <- lookup (x, y + 1)
      rest <- goRight d (n - 1) (x + 1) y
      return $ (x, y, left + leftUp + up) : rest
    goUp    !d !1 !x !y = do
      below <- lookup (x, y - 1)
      belowLeft <- lookup (x - 1, y - 1)
      rest <- goLeft (d + 1) (d + 1) x (y + 1)
      return $ (x, y, below + belowLeft) : rest
    goUp    !d !n !x !y = do
      left <- lookup (x - 1, y)
      below <- lookup (x, y - 1)
      belowLeft <- lookup (x - 1, y - 1)
      rest <- goUp d (n - 1) x (y + 1)
      return $ (x, y, left + belowLeft + below) : rest
    goLeft  !d !1 !x !y = do
      right <- lookup (x + 1, y)
      belowRight <- lookup (x + 1, y - 1)
      rest <- goDown d d (x - 1) y
      return $ (x, y, right + belowRight) : rest
    goLeft  !d !n !x !y = do
      right <- lookup (x + 1, y)
      belowRight <- lookup (x + 1, y - 1)
      below <- lookup (x, y - 1)
      rest <- goLeft d (n - 1) (x - 1) y
      return $ (x, y, right + belowRight + below) : rest
    goDown  !d !1 !x !y = do
      up <- lookup (x, y + 1)
      upRight <- lookup (x + 1, y + 1)
      rest <- goRight (d + 1) (d + 1) x (y - 1)
      return $ (x, y, up + upRight) : rest
    goDown  !d !n !x !y = do 
      up <- lookup (x, y + 1)
      upRight <- lookup (x + 1, y + 1)
      right <- lookup (x + 1, y)
      rest <- goDown d (n - 1) x (y - 1)
      return $ (x, y, up + upRight + right) : rest


main :: IO ()
main = defaultMain "2017.3" natural $ \n -> do
  let (x, y, val) = cells'' !! (n - 1)
  print $ abs x + abs y
  print $ take 10 cells''

{- 
 1:             ( 0, 0) 
 2: (+1,  0) -> ( 1, 0)
 3: ( 0, +1) -> ( 1, 1)
 4: (-1,  0) -> ( 0, 1)
 5: (-1,  0) -> (-1, 1)
 6: ( 0, -1) -> (-1, 0)
 7: ( 0, -1) -> (-1,-1)
 8: (+1,  0) -> ( 0,-1)
 9: (+1,)
-}