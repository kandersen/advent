{-# LANGUAGE TemplateHaskell #-}
module Advent.TwentySeventeen.Eighteen where

import Advent.Library

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Control.Monad.State
import Control.Monad.Identity

import Lens.Micro.Platform

type Register = Char

pRegister :: Parser Register 
pRegister = letterChar

type Expr = Either Register Int

pExpr :: Parser Expr
pExpr = Left <$> pRegister <|> Right <$> integer

data Command = Send Expr
             | Set Register Expr
             | Add Register Expr
             | Mul Register Expr
             | Mod Register Expr
             | Receive Register
             | JmpGZ Expr Expr
             deriving Show

pCommand :: Parser Command
pCommand = Send <$ string "snd " <*> pExpr
       <|> Set <$ string "set " <*> pRegister <* char ' ' <*> pExpr
       <|> Add <$ string "add " <*> pRegister <* char ' ' <*> pExpr
       <|> Mul <$ string "mul " <*> pRegister <* char ' ' <*> pExpr
       <|> Mod <$ string "mod " <*> pRegister <* char ' ' <*> pExpr
       <|> Receive <$ string "rcv " <*> pRegister
       <|> JmpGZ <$ string "jgz " <*> pExpr <* char ' ' <*> pExpr

type Registers = Map Register Int

freshRegisters :: Registers
freshRegisters = Map.fromList $ zip ['a'..'z'] (repeat 0)

data MachineState = MS { 
  _registers :: Registers,
  _frequencyBuffer :: Int,
  _pc :: Int
}
makeLenses ''MachineState

type MonadASM = StateT MachineState Identity

evaluateExpr :: Expr -> MonadASM Int
evaluateExpr (Right v) = return v
evaluateExpr (Left r) = fmap fromJust . use $ registers . at r

interpretCmd :: Command -> MonadASM ()
interpretCmd (Send e) = frequencyBuffer <~ evaluateExpr e
interpretCmd (Set r e) = do
  v <- evaluateExpr e
  registers . at r ?= v
interpretCmd (Add r e) = do
  v <- evaluateExpr e
  registers . at r %= (\(Just n) -> Just $ n + v)
interpretCmd (Mul r e) = do
  v <- evaluateExpr e
  registers . at r %= (\(Just n) -> Just $ n * v)
interpretCmd (Mod r e) = do
  v <- evaluateExpr e
  registers . at r %= (\(Just n) -> Just $ n `mod` v)
interpretCmd (Receive r) = undefined
  

main :: IO ()
main = defaultMain "2017.18" (pLines pCommand) print