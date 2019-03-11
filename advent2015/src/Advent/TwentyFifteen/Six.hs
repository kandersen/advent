{-# LANGUAGE FlexibleInstances #-}
module Advent.TwentyFifteen.Six(
  main,
  Pos,
  Cmd(..),
  pPos,
  pCommand
) where
import Advent.Library hiding (Pos)
import Control.Monad.State.Strict
import Data.Array
import Lens.Micro.Platform

type Pos = (Int,Int)
data Cmd = On     Pos Pos
         | Off    Pos Pos
         | Toggle Pos Pos 
         deriving Show

pPos :: Parser Pos
pPos = (,) <$> natural <* char ',' <*>  natural

pCommand :: Parser Cmd
pCommand = On <$ string "turn on " <*> pPos <* string " through " <*> pPos
       <|> Off <$ string "turn off " <*> pPos <* string " through " <*> pPos
       <|> Toggle <$ string "toggle " <*> pPos <* string " through " <*> pPos

rectangle :: Pos -> Pos -> [Pos]
rectangle (xfrom,yfrom) (xto,yto) = do
  x <- [xfrom .. xto]
  y <- [yfrom .. yto]
  return (x,y)

type Interpreter = State (Array Pos Bool)

runInterpreter :: Interpreter () -> Array Pos Bool
runInterpreter i = execState i init
  where
    init = array ((0,0),(999,999)) $ do
      x <- [0..999]
      y <- [0..999]
      return ((x,y), False)

interpretCmd :: Cmd -> Interpreter ()
interpretCmd (On from to) = 
  mapM_ (\p -> ix p .= True) (rectangle from to)
interpretCmd (Off from to) = 
  mapM_ (\p -> ix p .= False) (rectangle from to)
interpretCmd (Toggle from to) = 
  mapM_ (\p -> ix p .= True) (rectangle from to)
      
main :: IO ()
main = defaultMain "2016.6a" (pLines pCommand) $ \cmds -> do
  let lights = elems . runInterpreter . mapM_ interpretCmd $ cmds
  print $ length $ filter id lights
