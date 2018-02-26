{-# LANGUAGE Strict #-}
module Advent.TwentySeventeen.Seventeen where

import Advent.Library
import Data.Sequence (Seq, (<|), (|>), ViewL((:<)))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

type Circular a = Seq a

new :: [a] -> Circular a
new = Seq.fromList

display :: (Show a) => Circular a -> String
display s = case Seq.viewl s of
  x :< s' -> "(" ++ show x ++ ") " ++ (unwords . toList . fmap show $ s')

moveNext :: Circular a -> Circular a
moveNext s = case Seq.viewl s of
  x :< s' -> (s' |> x)

insert :: a -> Circular a -> Circular a
insert a = (a <|) . moveNext

readOut :: Circular a -> a
readOut = focus . moveNext

focus :: Circular a -> a 
focus s = case Seq.viewl s of
  x :< _ -> x

process :: Int -> Int -> Circular Int
process times steps = go times steps 1 (new [0])
  where
    go 0 _ _ b = b
    go t 0 v b = go (t - 1) steps (v + 1) (insert v b)
    go t s v b = go t (s - 1) v (moveNext b)

secondProcess :: Int -> Int -> Int
secondProcess times steps = go (times - 1) 2 1 1
  where
    go 0 _ _ v = v
    go t n p v =
      let nextPos = (p + steps) `mod` n in
      if nextPos == 0
        then go (t - 1) (n + 1) 1 n
        else go (t - 1) (n + 1) ((nextPos + 1) `mod` n) v

main :: IO ()
main = defaultMain "2017.17" natural $ \steps -> do
  print . readOut . process 2017 $ steps
  print . secondProcess 50000000 $ steps

