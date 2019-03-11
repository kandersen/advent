module Advent.TwentyFifteen.SixB where
import Advent.Library hiding (Pos)
import Data.List (sort, foldl')
import Advent.TwentyFifteen.Six

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
interpretCmd (On from to) = overlay (+1) from to
interpretCmd (Off from to) = overlay (const 0) from to
interpretCmd (Toggle from to) = overlay (+2) from to

main :: IO ()
main = defaultMain "2016.6" (pLines pCommand) $ \cmds -> do
  let grid = foldl' (flip ($)) allOff . map interpretCmd $ cmds
  print . foldl' (+) 0 $ do
    x <- [0..999]
    y <- [0..999]
    return $ grid (x,y)
