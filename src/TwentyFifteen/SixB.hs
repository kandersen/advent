module SixB where
import Advent
import Data.List (sort, foldl')

type Pos = (Int,Int)
data Inst = On | Off | Toggle deriving Show
type Cmd = (Inst, Pos, Pos)

type LitShape = Pos -> Int

allOff :: LitShape
allOff = const 0

overlay :: (Int -> Int) -> Pos -> Pos -> LitShape -> LitShape
overlay f (x, y) (a, b) under p@(p1, p2) =
  if left <= p1 && p1 <= right && bot <= p2 && p2 <= top
  then f $ under p
  else under p
    where
     [left, right] = sort [x,a]
     [bot,    top] = sort [y,b]

interpretCmd :: Cmd -> (LitShape -> LitShape)
interpretCmd (On,from,to) = overlay (+1) from to
interpretCmd (Off,from,to) = overlay ((max 0) . pred) from to
interpretCmd (Toggle,from,to) = overlay (+2) from to

parseLine :: String -> Cmd
parseLine s = case words s of
  ["turn","on",from,_,to] -> (On,read ("(" ++ from ++ ")"), read ("(" ++ to ++ ")"))
  ["turn","off",from,_,to] -> (Off,read ("(" ++ from ++ ")"), read ("(" ++ to ++ ")"))
  ["toggle",from,_,to] -> (Toggle,read ("(" ++ from ++ ")"), read ("(" ++ to ++ ")"))
  where

main :: IO ()
main = defaultMain $ \input -> do
  let grid = foldl' (flip ($)) allOff . map (interpretCmd . parseLine) . lines $ input
  print . foldl' (+) 0 $ do
    x <- [0..999]
    y <- [0..999]
    return $ grid (x,y)
