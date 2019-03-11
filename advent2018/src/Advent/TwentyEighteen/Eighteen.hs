{-# LANGUAGE TupleSections #-}
module Advent.TwentyEighteen.Eighteen where

import Advent.Library

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Comonad
import Data.Maybe (fromJust)

data Acre = Open | Tree | Lumberyard deriving (Show, Eq)

pAcre :: Parser Acre
pAcre = Open <$ char '.'
    <|> Tree <$ char '|'
    <|> Lumberyard <$ char '#'

newtype Grid k a = G { unG :: (Map k a, k) }

instance Functor (Grid k) where
  fmap f (G (m, i)) = G (fmap f m, i)

instance Ord k => Comonad (Grid k) where
  extract (G (m, i)) = m Map.! i
  duplicate (G (v, i)) = G (Map.mapWithKey (\j -> const $ G (v, j)) v, i)  

  -- extract . duplicate      = id
  -- fmap extract . duplicate = id
  -- duplicate . duplicate    = fmap duplicate . duplicate

--   extract (duplicate (m, i))
-- = extract (mapWithKey (\j -> const (m, j)) m, i)  
-- = mapWithKey (\j -> const (m, j)) m ! i
-- = (m, i)

--   fmap extract (duplicate (mv i)) 
-- = fmap extract (mapWithKey (\j -> const (v, j)) v, i)
-- = (fmap extract (mapWithKey (\j -> const (v, j)) v), i)
-- <=>
--   fmap extract (mapWithKey (\j -> const (v, j)) v)
-- = mapWithKey (\j -> const (v ! j) v
-- = v
--   duplicate (duplicate (m, i))
-- = duplicate (mapWithKey (\j -> const (m, j)) m, i)
-- = 

neighbourhood :: Ord k => (k -> [k]) -> a -> Grid k a -> [a]
neighbourhood f def (G (m, i)) = [ Map.findWithDefault def j m | j <- f i ]
---[ Problem ]-----------------------------------------------------------------

type Rule = Grid (Int,Int) Acre -> Acre

step :: Rule -> Grid (Int,Int) Acre -> Grid (Int,Int) Acre
step = extend

adjecents :: (Int, Int) -> [(Int,Int)]
adjecents (x,y) = [
  (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
  (x - 1, y),                 (x + 1, y),
  (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

rule :: Rule
rule ps = case extract ps of
    Open -> if (>= 3) . length . filter (== Tree) $ pattern then Tree else Open
    Tree -> if (>= 3) . length . filter (== Lumberyard) $ pattern then Lumberyard else Tree
    Lumberyard -> if Tree `elem` pattern then Lumberyard else Open
    where
      pattern :: [Acre]
      pattern = neighbourhood adjecents Open ps

mkArea :: [Acre] -> Grid (Int,Int) Acre
mkArea xs = G (Map.fromList points, (0,0))
  where
    points = zip [(x,y) | y <- [0..49], x <- [0..49]] xs
    
run :: [Acre] -> [Grid (Int,Int) Acre]
run acres = 
  iterate (step rule) (mkArea acres)

value :: Grid (Int,Int) Acre -> Int
value g = trees * yards
  where
    acres = Map.elems . fst . unG $ g
    trees = length . filter (== Tree) $ acres
    yards = length . filter (== Lumberyard) $ acres

main :: IO ()
main = defaultMain "2018.18" (concat <$> pLines (some pAcre)) $ \acres -> do
  print $ neighbourhood adjecents Open $ mkArea acres
  print . value $ run acres !! 11