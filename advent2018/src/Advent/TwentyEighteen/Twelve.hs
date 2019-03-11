{-# LANGUAGE TupleSections #-}
module Advent.TwentyEighteen.Twelve where

import Advent.Library
import Control.Monad
import Control.Comonad
import Data.Maybe (fromMaybe)

---[ Parsing ]-----------------------------------------------------------------

pState :: Parser Bool
pState = False <$ char '.'
     <|> True <$ char '#'

pInitialState :: Parser [Bool]
pInitialState = string "initial state: " *> some pState

pRule :: Parser ([Bool], Bool)
pRule = (,) <$> replicateM 5 pState <* string " => " <*> pState

pProblem :: Parser ([Bool], [([Bool], Bool)])
pProblem = (,) <$> pInitialState <* eol <* eol <*> some (pRule <* eol)

---[ Comonad Stuff ]-----------------------------------------------------------

type Grid a = [a]

newtype FocusedGrid a = FG { unFG :: (Grid a, Int) }

grid :: FocusedGrid a -> Grid a
grid = fst . unFG

instance Functor FocusedGrid where
  fmap f (FG (xs, i)) = FG (fmap f xs, i)

instance Comonad FocusedGrid where
  -- extract :: FocusedGrid a -> a
  extract (FG (xs, i)) = xs !! i

  --  duplicate :: FocusedGrid a -> FocusedGrid (FocusedGrid a)
  duplicate (FG (xs, i)) = FG (FG . (xs,) <$> [0..length xs - 1], i)
  
  
neighbourhood :: (Int -> [Int]) -> a -> FocusedGrid a -> [a]
neighbourhood f def (FG (xs, i)) = [fromMaybe def (xs !!? j) | j <- f i]

---[ Problem ]-----------------------------------------------------------------

type Plants = FocusedGrid Bool
type Rule = Plants -> Bool

ppPlants :: Plants -> String
ppPlants = fmap ppPot . grid
  where
    ppPot p = if p then '#' else '.'

step :: Rule -> Plants -> Plants
step = extend

mkRule :: [([Bool], Bool)] -> Rule
mkRule rs ps = maybe (error $ "undefined pattern: " ++ show pattern) id $ 
  lookup pattern rs
    where
      pattern :: [Bool]
      pattern = neighbourhood (\i -> [i - 2, i - 1, i, i + 1, i + 2]) False ps

mkPlants :: Int -> [Bool] -> Plants
mkPlants padding xs = FG $ (replicate padding False ++ 
                            xs ++ 
                            replicate padding False
                          , padding)

run :: [([Bool], Bool)] -> [Bool] -> Int -> [Plants]
run rules initialState padding = 
  iterate (step (mkRule rules)) (mkPlants padding initialState)

similar :: Plants -> Plants -> Bool
similar p1 p2 = and $ zipWith (==) (dropWhile not (grid p1)) (dropWhile not (grid p2))

potNumbers :: Plants -> [Int]
potNumbers (FG (xs, origin)) = [ i | (True, i) <- zip xs [0 - origin ..]]

---[ App ]---------------------------------------------------------------------

main :: IO () 
main = defaultMain "2018.12" pProblem $ \(initialState, rules) -> do
  print . sum . potNumbers $ run rules initialState 20 !! 20
  let (pots, n) = fixedPoint (\p1 p2 -> similar (fst p1) (fst p2)) $ zip (run rules initialState 100) [0..]
  print $ sum . fmap  (+ (50000000000 - n)) . potNumbers $ pots
