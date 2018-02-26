{-# LANGUAGE RankNTypes #-}
module Advent.TwentySeventeen.Five where

import Advent.Library

import Control.Monad.ST
import Data.Array.ST

type Memory s = STArray s Int Int

run :: (Int -> Int) -> [Int] -> Int
run incrementRule initMem = runST $ do
  mem <- newListArray (0, length initMem - 1) initMem
  go mem 0 0

  where
    inbounds n = 0 <= n && n <= length initMem - 1

    fetchAndInc :: Int -> Memory s -> ST s Int
    fetchAndInc pc mem = do
      res <- readArray mem pc
      writeArray mem pc (incrementRule res)
      return res

    go :: Memory s -> Int -> Int -> ST s Int
    go mem pc steps | not $ inbounds pc = return steps
                    | otherwise = do
                        jmp <- fetchAndInc pc mem
                        go mem (pc + jmp) (steps + 1)

main :: IO ()
main = defaultMain "2017.5" (pLines integer) $ \prog -> do
  print . run (+1) $ prog
  print . run (\n -> if n > 2 then n - 1 else n + 1) $ prog