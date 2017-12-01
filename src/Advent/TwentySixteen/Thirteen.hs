module Thirteen(main) where

import Data.Bits
import Control.Lens hiding ((:<))
import Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.State
import Control.Monad.Except

type Pos = (Int,Int)

type Secret = Int

isOdd :: Int -> Bool
isOdd = (/= 0) . (`mod` 2)

isWall :: Secret -> Pos -> Bool
isWall secret (x, y) =
  isOdd . popCount $ x*x + 3*x + 2*x*y + y + y*y + secret

successors :: Pos -> [Pos]
successors (0,0) = [(0,1),(1,0)]
successors (0,y) = [(0, y - 1), (1, y), (0, y + 1)]
successors (x,0) = [(x - 1, 0), (x, 1), (x + 1, 0)]
successors (x,y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

shortestPathFromTo :: (Pos -> Bool) -> Pos -> Pos -> Int
shortestPathFromTo wallPred origin dst =
  case evalStateT go (Set.empty, Seq.singleton (0,origin)) of
    Left d -> d
    Right _ -> error $ "shouldn't happen"
  where
    go :: StateT (Set Pos, Seq (Int, Pos)) (Either Int) ()
    go = do
      (d,pos) :< q' <- Seq.viewl <$> use _2
      _2 .= q'
      visited <- use _1
      let succs = filter ((&&) <$> not . wallPred <*> not . flip Set.member visited) $ successors pos
      forM_ succs $ \s ->
        if s == dst
        then lift . Left $ d + 1
        else do
          _1 %= Set.insert s
          _2 %= (|> (d + 1, s))
      go

reachableFromIn :: (Pos -> Bool) -> Pos -> Int -> Int
reachableFromIn wallPred origin dist = Set.size . fst $ execState go (Set.empty, Seq.singleton (0,origin))
  where
    go :: State (Set Pos, Seq (Int, Pos)) ()
    go = do
      q <- Seq.viewl <$> use _2
      case q of
        EmptyL -> return ()
        (d,pos) :< q' -> do
          _2 .= q'
          if d == dist
            then go
            else do
              visited <- use _1
              let succs = filter ((&&) <$> not . wallPred <*> not . flip Set.member visited) $ successors pos
              forM_ succs $ \s -> do
                _1 %= Set.insert s
                _2 %= (|> (d + 1, s))
              go

main :: IO ()
main = do
  secret <- read <$> getContents
  putStrLn "----[ Part  I ]----"
  print $ shortestPathFromTo (isWall secret) (1,1) (31,39)
  putStrLn "----[ Part II ]----"
  print $ reachableFromIn (isWall secret) (1,1) 50
