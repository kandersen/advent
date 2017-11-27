{-# LANGUAGE FlexibleInstances #-}
module Six where
import Advent
import Control.Monad.State.Strict
import Data.Array
import Lens.Micro
import Lens.Micro.GHC
import Lens.Micro.Mtl



rectangle :: Pos -> Pos -> [Pos]
rectangle (xfrom,yfrom) (xto,yto) = do
  x <- [xfrom..xto]
  y <- [yfrom..yto]
  return (x,y)

type Pos = (Int,Int)
data Inst = On | Off | Toggle deriving Show
type Cmd = (Inst, Pos, Pos)

type Interpreter = State (Array Pos Bool)

runInterpreter :: Interpreter () -> Array Pos Bool
runInterpreter i = execState i init
  where
    init = array ((0,0),(999,999)) $ do
      x <- [0..999]
      y <- [0..999]
      return ((x,y), False)

interpretInst :: Inst -> (Pos -> Interpreter ())
interpretInst On p = ix p .= True
interpretInst Off p = ix p .= False
interpretInst Toggle p = ix p %= not

interpretCmd :: Cmd -> Interpreter ()
interpretCmd (inst,from,to) =
  sequence_ $ map (interpretInst inst) (rectangle from to)

parseLine :: String -> Cmd
parseLine s = case words s of
  ["turn","on",from,_,to] -> (On,read ("(" ++ from ++ ")"), read ("(" ++ to ++ ")"))
  ["turn","off",from,_,to] -> (Off,read ("(" ++ from ++ ")"), read ("(" ++ to ++ ")"))
  ["toggle",from,_,to] -> (Toggle,read ("(" ++ from ++ ")"), read ("(" ++ to ++ ")"))
  where


main :: IO ()
main = defaultMain $ \input -> do
  let lights = elems . runInterpreter . sequence_ . map (interpretCmd . parseLine) . lines $ input
  print $ length $ filter id lights
