{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Advent.TwentySixteen.Twelve(main) where

import Advent.Library
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IM
import Lens.Micro.Platform
import Control.Monad.Trans.State
import Control.Applicative

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

data Instr = ICopy (Either Int Register) Register
           | IInc Register
           | IDec Register
           | IJnz (Either Int Register) Int
           deriving Show

pInc :: Parser Instr
pInc = IInc <$ string "inc" <* space <*> pRegister

pDec :: Parser Instr
pDec = IDec <$ string "dec" <* space <*> pRegister

pCopy :: Parser Instr
pCopy = ICopy <$ string "cpy" <* space <*> (Right <$> pRegister <|> Left <$> integer) <* space <*> pRegister

pJnz :: Parser Instr
pJnz = IJnz <$ string "jnz" <* space <*> (Right <$> pRegister <|> Left <$> integer) <* space <*> integer

pInstr :: Parser Instr
pInstr = pInc <|> pDec <|> pCopy <|> pJnz
  
data CFG = Copy (Either Int Register) Register CFG
         | Inc Register CFG
         | Dec Register CFG
         | Jnz (Either Int Register) CFG CFG
         | Done

parseCFG' :: [Instr] -> CFG
parseCFG' is = node 0
  where
    node :: Int -> CFG
    node n = case IM.lookup n cache of
      Nothing -> Done
      Just c -> c

    cache :: IntMap CFG
    cache = IM.fromList [ (n, build n i) | (n,i) <- zip [0..] is ]

    build :: Int -> Instr -> CFG
    build a (ICopy src dst) = Copy src dst (node $ a + 1)
    build a (IInc reg) = Inc reg (node $ a + 1)
    build a (IDec reg) = Dec reg (node $ a + 1)
    build a (IJnz src jmp) = Jnz src (node $ a + jmp) (node $ a + 1)

data Registers = Registers {
  _a :: Int,
  _b :: Int,
  _c :: Int,
  _d :: Int
  }
makeLenses ''Registers

executeCFG :: Registers -> CFG -> Int
executeCFG initRegs cfg = execState (run cfg) initRegs ^. a
  where
    loc :: Register -> Lens' Registers Int
    loc A = a
    loc B = b
    loc C = c
    loc D = d

    read :: Either Int Register -> State Registers Int
    read (Left n) = return n
    read (Right r) = use $ loc r

    run :: CFG -> State Registers ()
    run Done = return ()
    run (Inc r k) = do
      loc r += 1
      run k
    run (Dec r k) = do
      loc r -= 1
      run k
    run (Copy src dst k) = do
      val <- read src
      loc dst %= const val
      run k
    run (Jnz src jmpk k) = do
      val <- read src
      run $ if val /= 0
        then jmpk
        else k

main :: IO ()
main = defaultMain "2016.12" (parseCFG' <$> pLines pInstr) $ \program -> do
  putStrLn "----[ Part  I ]----"
  print $ executeCFG (Registers 0 0 0 0) program
  putStrLn "----[ Part II ]----"
  print $ executeCFG (Registers 0 0 1 0) program

