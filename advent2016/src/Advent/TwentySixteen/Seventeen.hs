{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Advent.TwentySixteen.Seventeen(main) where

import Advent.Library hiding (Pos)
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.Trans
import Lens.Micro

import Data.Sequence (Seq, ViewL(..), (|>), (><))
import qualified Data.Sequence as Seq

import Data.Maybe (fromJust, catMaybes, mapMaybe)

type Pos = (Int,Int)

isOpen :: Char -> Bool
isOpen 'b' = True
isOpen 'c' = True
isOpen 'd' = True
isOpen 'e' = True
isOpen 'f' = True
isOpen   _ = False

neighbors :: Pos -> [Maybe Pos]
neighbors (1,1) = [Nothing, Just (1,2), Nothing, Just (2, 1)]
neighbors (1,4) = [Just  (1,3), Nothing, Nothing, Just (2, 4)]
neighbors (1,y) = [Just (1, y - 1), Just (1, y + 1), Nothing, Just (2,y)]
neighbors (4,1) = [Nothing, Just (4,2), Just (3,1), Nothing]
neighbors (x,1) = [Nothing, Just (x, 2), Just (x - 1, 1), Just (x + 1, 1)]
neighbors (x,y) = [Just (x, y - 1), Just (x, y + 1), Just (x - 1, y), Just (x + 1, y)]

mpair :: Maybe a -> b -> Maybe (a, b)
mpair Nothing _ = Nothing
mpair (Just a) b = Just (a, b)

done :: Pos -> Bool
done (4,4) = True
done _ = False

search :: String -> String
search seed = case evalStateT go (Seq.singleton ((1,1), [], 0)) of
  Left path -> path
  where
    go :: StateT (Seq (Pos, String, Int)) (Either String) ()
    go = do
      (pos,path, dist) <- fromJust <$> pop
      let u:d:l:r:_ = md5hash $ seed ++ path
      let next = zip3 (neighbors pos) ['U','D','L','R'] [u, d, l, r] 
      let nexts = mapMaybe (\ (n, c, _) -> mpair n c) . filter (isOpen . (^. _3)) $ next
      forM_ nexts $ \(p,c) ->
        if done p
        then lift . Left $ snoc path c
        else push (p, snoc path c, dist + 1)
      go

type Q = Seq (Pos, String, Int)

search' :: String -> [String]
search' seed = exhaustiveSearch (Seq.singleton ((1,1), [], 0))
  where
    exhaustiveSearch :: Q -> [String]
    exhaustiveSearch q = case evalStateT go q of
      Left (p, q') -> p : exhaustiveSearch q'
      Right () -> []

    go :: StateT (Seq (Pos, String, Int)) (Either (String, Seq (Pos, String, Int))) ()
    go = do
      (pos,path, dist) <- fromJust <$> pop
      let u:d:l:r:_ = md5hash $ seed ++ path
      let next = zip3 (neighbors pos) ['U','D','L','R'] [u, d, l, r] 
      let nexts = mapMaybe (\ (n, c, _) -> mpair n c) . filter (isOpen . (^. _3)) $ next
      forM_ nexts $ \(p,c) ->
        if done p
        then do
          q <- get
          lift . Left $ (snoc path c, q)
        else push (p, snoc path c, dist + 1)
      go


main :: IO ()
main = do
  seed <- getLine
  putStrLn $ search seed
  print $ maximum . map length . search' $ seed
