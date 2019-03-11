{-# LANGUAGE RankNTypes #-}
module Advent.TwentyEighteen.Nineteen where

import Advent.Library
import Control.Monad
import Lens.Micro.Platform
import Data.Bits ((.&.), (.|.))
import Data.Map (Map)

type Reg = Int

pReg :: Parser Reg 
pReg = read . (:[]) <$> oneOf "012345"

data Op = Addr | Addi 
        | Mulr | Muli
        | Banr | Bani
        | Borr | Bori
        | Setr | Seti
        | Gtir | Gtri | Gtrr
        | Eqir | Eqri | Eqrr
        deriving (Eq, Enum, Bounded, Show)

pOp :: Parser Op
pOp = foldr1 (<|>) [ op <$ string s | (s, op) <- opTable ]
  where
    opTable = 
      [("addr", Addr)
      ,("addi", Addi)
      ,("mulr", Mulr)
      ,("muli", Muli)
      ,("banr", Banr)
      ,("bani", Bani)
      ,("borr", Borr)
      ,("bori", Bori)
      ,("setr", Setr)
      ,("seti", Seti)
      ,("gtir", Gtir)
      ,("gtri", Gtri)
      ,("gtrr", Gtrr)
      ,("eqir", Eqir)
      ,("eqri", Eqri)
      ,("eqrr", Eqrr)
      ]
        
---
  
type Registry = [Int]

ops :: [Op]
ops = [Addr .. Eqrr]

reg :: Int -> Lens' Registry (Maybe Int)
reg = at

interp :: Op -> Reg -> Reg -> Reg -> (Registry -> Registry)
interp op a b c rs = rs & reg c ?~ case op of
  Addr -> (rs ^?! reg a) + (rs ^?! reg b)
  Addi -> (rs ^?! reg a) + b
  Mulr -> (rs ^?! reg a) * (rs ^?! reg b)
  Muli -> (rs ^?! reg a) * b
  Banr -> (rs ^?! reg a) .&. rs ^?! reg b
  Bani -> (rs ^?! reg a) .&. b
  Borr -> (rs ^?! reg a) .|. (rs ^?! reg b)
  Bori -> (rs ^?! reg a) .|. b
  Setr -> rs ^?! reg a
  Seti -> a
  Gtir -> if a > (rs ^?! reg b) then 1 else 0
  Gtri -> if (rs ^?! reg a) > b then 1 else 0
  Gtrr -> if (rs ^?! reg a) > (rs ^. reg b) then 1 else 0
  Eqir -> if a == (rs ^?! reg b) then 1 else 0
  Eqri -> if (rs ^?! reg a) == b then 1 else 0
  Eqrr -> if (rs ^?! reg a) == (rs ^?! reg b) then 1 else 0

data Cmd = BindIP Int
         | Cmd Op Reg Reg Reg
         deriving Show

pCmd :: Parser Cmd
pCmd = BindIP <$ string "#ip" <* space <*> pReg
   <|> Cmd <$> pOp <* space <*> pReg <* space <*> pReg <* space <*> pReg

main :: IO ()
main = defaultMain "2018.19" (pLines pCmd) $ print . length