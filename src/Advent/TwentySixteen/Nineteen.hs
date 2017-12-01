{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Nineteen(main) where

import Advent
import Control.Monad.State

import Data.Maybe

import Data.Sequence (Seq, ViewL(..), (|>), (><))
import qualified Data.Sequence as Seq

initial :: Int -> Seq Int
initial = flip Seq.fromFunction (+1)

solve :: Seq Int -> Int
solve = evalState go
  where
    go = do
      a <- fromJust <$> pop
      pop >>= \case
        Nothing -> return a
        Just _ -> do
          modify (|> a)
          go

deleteAt :: Int -> Seq a -> Seq a
deleteAt i s =
  let (hd,tl) = Seq.splitAt i s in
  case Seq.viewl tl of
    _ :< tl' -> hd >< tl'
    _ -> s

solve' :: Seq Int -> Int
solve' = evalState go
  where
    go :: State (Seq Int) Int
    go =
      length <$> get >>= \case
        1 -> flip Seq.index 0 <$> get
        elves -> do
          a <- fromJust <$> pop
          modify . deleteAt $ elves `div` 2 - 1
          modify (|> a)
          go

main :: IO ()
main = do
  initialElves <- initial . read <$> getLine
  putStrLn "----[ Part  I ]----"
  print $ solve initialElves
  putStrLn "----[ Part II ]----"
  print $ solve' initialElves
