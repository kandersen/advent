{-# LANGUAGE RankNTypes #-}
module Advent.TwentyEighteen.Sixteen where

import Advent.Library
import Control.Monad
import Lens.Micro.Platform
import Data.Bits ((.&.), (.|.))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intersect, (\\))

type Reg = Int

type Regs = (Int, Int, Int, Int)

type Inst = (Int, Reg, Reg, Reg)


pReg :: Parser Reg 
pReg = read . (:[]) <$> oneOf "0123"

quartuplet :: [a] -> (a, a, a, a)
quartuplet [a, b, c, d] = (a, b, c, d)

pInst :: Parser Inst
pInst = (,,,) <$> natural <* space <*> pReg <* space <*> pReg <* space <*> pReg

type Sample = (Regs, Inst, Regs)

opcode :: Sample -> Int
opcode (_, (oc, _, _, _), _) = oc

pRegs :: Parser Regs
pRegs = quartuplet <$> brackets (natural `sepBy` string ", ")

pSample :: Parser Sample
pSample = (,,) <$ string "Before:" <* space <*> pRegs <* eol
              <*> pInst <* eol
               <* string "After:" <* space <*> pRegs

pProblem :: Parser ([Sample], [Inst])
pProblem = (,) <$> some (pSample <* space) <*> some (pInst <* space)

--- 

data Op = Addr | Addi 
        | Mulr | Muli
        | Banr | Bani
        | Borr | Bori
        | Setr | Seti
        | Gtir | Gtri | Gtrr
        | Eqir | Eqri | Eqrr
        deriving (Eq, Enum, Bounded, Show)

ops :: [Op]
ops = [Addr .. Eqrr]

reg :: Int -> Lens' Regs Int
reg 0 = _1
reg 1 = _2
reg 2 = _3
reg 3 = _4

interp :: Op -> Reg -> Reg -> Reg -> (Regs -> Regs)
interp op a b c rs = rs & reg c .~ case op of
  Addr -> (rs ^. reg a) + (rs ^. reg b)
  Addi -> (rs ^. reg a) + b
  Mulr -> (rs ^. reg a) * (rs ^. reg b)
  Muli -> (rs ^. reg a) * b
  Banr -> (rs ^. reg a) .&. rs ^. reg b
  Bani -> (rs ^. reg a) .&. b
  Borr -> (rs ^. reg a) .|. (rs ^. reg b)
  Bori -> (rs ^. reg a) .|. b
  Setr -> rs ^. reg a
  Seti -> a
  Gtir -> if a > (rs ^. reg b) then 1 else 0
  Gtri -> if (rs ^. reg a) > b then 1 else 0
  Gtrr -> if (rs ^. reg a) > (rs ^. reg b) then 1 else 0
  Eqir -> if a == (rs ^. reg b) then 1 else 0
  Eqri -> if (rs ^. reg a) == b then 1 else 0
  Eqrr -> if (rs ^. reg a) == (rs ^. reg b) then 1 else 0

candidates :: Sample -> [Op]
candidates (before, (_, a, b, c), after) = do
    op <- ops
    if interp op a b c before == after
      then return op
      else mzero

solve :: Map Int [Op] -> Map Int Op
solve = fmap (either (error . show) id) . go . fmap Left
  where
    go ms = 
      case [ (k, a) | (k, Left [a]) <- Map.assocs ms ] !!? 0 of
        Nothing -> ms
        Just (k, a) -> do
          let ms' = fmap (removeCandidate a) ms
          let ms'' = Map.insert k (Right a) ms'
          go ms''

    removeCandidate :: Op -> Either [Op] Op -> Either [Op] Op
    removeCandidate a (Left cs) = Left $ cs \\ [a]
    removeCandidate _ r         = r

analyze :: [Sample] -> Map Int Op
analyze = solve . go Map.empty
  where
    go :: Map Int [Op] -> [Sample] -> Map Int [Op]
    go = foldl (\m s -> Map.alter (update (candidates s)) (opcode s) m)

    update :: [Op] -> Maybe [Op] -> Maybe [Op]
    update candidates Nothing = Just candidates
    update candidates (Just cs) = Just $ candidates `intersect` cs
    
execute :: Map Int Op -> [Inst] -> Regs
execute opMap = applyAll (0, 0, 0, 0) . fmap go
  where
    go (op, a, b, c) = interp (opMap Map.! op) a b c

--- 

main :: IO ()
main = defaultMain "2018.16" pProblem $ \(samples, prog) -> do
    print $ length . filter ((>= 3) . length) $ candidates <$> samples
    let opMap = analyze samples
    print $ (^. reg 0) . execute opMap $ prog