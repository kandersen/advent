{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Twentythree where

import Advent
import Control.Monad.Identity
import Control.Monad.State
import Data.Bool (bool)
import Lens.Micro
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Lens.Micro.TH

type Reg = Bool

data Inst = Hlf Reg
          | Tpl Reg
          | Inc Reg
          | Jmp Int
          | Jie Reg Int
          | Jio Reg Int
          deriving Show

data MState = MState {
  _instPtr :: Int,
  _code :: [Inst],
  _a :: Integer,
  _b :: Integer }

makeLenses ''MState

reg :: Reg -> Lens' MState Integer
reg = bool b a

type Computer a = State MState a

isEven :: Integral a => a -> Bool
isEven = (== 0) . (`mod` 2)

execute :: Inst -> Computer ()
execute (Hlf r) = do
  reg r %= (`div` 2)
  instPtr += 1
execute (Tpl r) = do
  reg r %= (* 3)
  instPtr += 1
execute (Inc r) = do
  reg r %= succ
  instPtr += 1
execute (Jmp o) = do
  instPtr += o
execute (Jie r o) = do
  v <- use $ reg r
  instPtr += bool 1 o (isEven v)
execute (Jio r o) = do
  v <- use $ reg r
  instPtr += bool 1 o (v == 1)

runComputer :: Computer (Integer,Integer)
runComputer = do
  inst <- use instPtr
  code <- use code
  case code ^? ix inst  of
    Nothing -> (,) <$> use a <*> use b
    Just op -> do
      execute op
      runComputer

runProgram :: [Inst] -> (Integer,Integer)
runProgram prog = evalState runComputer initState
  where
    initState = MState 0 prog 1 0

parseLine :: String -> Inst
parseLine xs = case words xs of
  ["hlf",reg] -> Hlf (reg == "a")
  ["tpl",reg] -> Tpl (reg == "a")
  ["inc", reg] -> Inc (reg == "a")
  ["jmp",offset] -> Jmp (parseOffset offset)
  ["jie",regcomma,offset] -> Jie (regcomma == "a,") (parseOffset offset)
  ["jio",regcomma,offset] -> Jio (regcomma == "a,") (parseOffset offset)

parseOffset :: String -> Int
parseOffset ('+':xs') = read xs'
parseOffset xs        = read xs

main :: IO ()
main = defaultMain $ \input -> do
  let program = map parseLine . lines $ input
  let finalState = runProgram program
  print $ snd finalState
