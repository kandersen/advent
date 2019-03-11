{-# LANGUAGE RankNTypes #-}
module Advent.TwentySeventeen.Six where

import Advent.Library
import Advent.Library.Trie (Trie)
import qualified Advent.Library.Trie as Trie

import Control.Monad.ST
import Data.Array.ST
import Data.List (maximumBy)
import Data.Ord (comparing)

type Stacks s = STArray s Int Int

run :: [Int] -> Int
run initStacks = runST $ do
  stacks <- newListArray (0, length initStacks - 1) initStacks
  i <- findLargestStack stacks
  go 0 (Trie.singleton $ Just 0) stacks i

  where
    nextIndex n = (n + 1) `mod` length initStacks

    findLargestStack :: Stacks s -> ST s Int
    findLargestStack ss = 
      fst . maximumBy (comparing snd) <$> getAssocs ss

    takeArray :: Stacks s -> Int -> ST s Int
    takeArray stacks i = do
      res <- readArray stacks i
      writeArray stacks i 0
      return res

    incrArray :: Stacks s -> Int -> ST s ()
    incrArray s i = readArray s i >>= writeArray s i . (+1)

    go count seen stacks i = do
      config <- getElems stacks
      Trie.insert''' config count seen (return count) $ \seen' -> do
        vs <- takeArray stacks i
        redistribute count seen' vs stacks (nextIndex i)

    redistribute count seen 0 stacks _ = do
      i <- findLargestStack stacks
      go (count + 1) seen stacks i
    redistribute count seen n stacks i = do
      incrArray stacks i
      redistribute count seen (n - 1) stacks (nextIndex i)


main :: IO ()
main = defaultMain "2017.6" (natural `sepBy` tab) $ \prog -> do
  print . run $ [0, 2, 7, 0]
  print . run $ prog