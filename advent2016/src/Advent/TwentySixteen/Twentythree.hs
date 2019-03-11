{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Advent.TwentySixteen.Twentythree(main) where

import Advent.Library
import Text.Megaparsec hiding (State)
import Lens.Micro.Platform
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Applicative
import Control.Monad.ST
import Control.Monad

import Data.Array
import Data.Array.ST 


data Register = A | B | C | D
              deriving Show

pRegister :: Parser Register
pRegister = do
  c <- oneOf ['a','b','c','d']
  case c of
    'a' -> return A
    'b' -> return B
    'c' -> return C
    'd' -> return D
    _ -> empty

data Instr = IInc Register
           | IDec Register
           | ITgl Register
           | IJnz (Either Int Register) (Either Int Register)
           | ICopy (Either Int Register) (Either Int Register)
           deriving Show

toggle :: Instr -> Instr
toggle (IInc r) = IDec r
toggle (IDec r) = IInc r
toggle (ITgl r) = IInc r
toggle (ICopy src dst) = IJnz src dst
toggle (IJnz src dst) = ICopy src dst

pIntOrReg :: Parser (Either Int Register)
pIntOrReg = Right <$> pRegister <|> Left <$> integer

pInc :: Parser Instr
pInc = IInc <$ string "inc" <* space <*> pRegister

pDec :: Parser Instr
pDec = IDec <$ string "dec" <* space <*> pRegister

pCopy :: Parser Instr
pCopy = ICopy <$ string "cpy" <* space <*> pIntOrReg <* space <*> pIntOrReg

pJnz :: Parser Instr
pJnz = IJnz <$ string "jnz" <* space <*> pIntOrReg <* space <*> pIntOrReg

pTgl :: Parser Instr
pTgl = ITgl <$ string "tgl" <* space <*> pRegister


pInstr :: Parser Instr
pInstr = pInc <|> pDec <|> pCopy <|> pJnz <|> pTgl

data Registers = Registers {
  _a :: Int,
  _b :: Int,
  _c :: Int,
  _d :: Int
  }
makeLenses ''Registers

execute :: Registers -> [Instr] -> Int
execute initRegs prog = runST initialize
  where
    inbounds n = 1 <= n && n <= length prog
    
    initialize :: forall s. ST s Int
    initialize = do
      mem <- newListArray (1, length prog) prog
      finalRegs <- execStateT (go mem 1) initRegs
      return $ finalRegs ^. a

    loc :: Register -> Lens' Registers Int
    loc A = a
    loc B = b
    loc C = c
    loc D = d

    read :: Either Int Register -> StateT Registers (ST s) Int
    read (Left n) = return n
    read (Right r) = use $ loc r

    peek :: Int -> STArray s Int Instr -> Int -> StateT Registers (ST s) [Instr]
    peek _   _ 0 = return []
    peek a mem n | inbounds a = (:) <$> lift (readArray mem a) <*> peek (a + 1) mem (n - 1)
                 | otherwise = return []

    go :: STArray s Int Instr -> Int -> StateT Registers (ST s) ()
    go mem pc | not (inbounds pc) = return ()
              | otherwise = do
      inst <- lift $ readArray mem pc
      case inst of
        IInc r -> do
          lookahead <- peek (pc + 1) mem 2 
          case lookahead of
            [IDec r, IJnz src (Left (-2))] ->
              undefined
            _ -> do
              loc r += 1
              go mem (pc + 1)
        IDec r -> do
          loc r -= 1
          go mem (pc + 1)
        ITgl r -> do
          offset <- use $ loc r
          when (inbounds $ pc + offset) $
            lift (readArray mem (pc + offset) >>= writeArray mem (pc + offset) . toggle)
          go mem (pc + 1)
        ICopy _ (Left _) -> 
          go mem (pc + 1)
        ICopy src (Right r) -> do
          val <- read src
          loc r .= val
          go mem (pc + 1)
        IJnz src jmp -> do
          val <- read src
          if val /= 0
            then do
              offset <- read jmp
              go mem (pc + offset)
            else
              go mem (pc + 1)

main :: IO ()
main = defaultMain "2016.23" (pLines pInstr) $ \program -> do
  putStrLn "----[ Part  I ]----"
  print $ execute (Registers 12 0 0 0) program
  --putStrLn "----[ Part II ]----"
  --print $ execute (Registers 0 0 1 0) program

