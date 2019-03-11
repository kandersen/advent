{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Advent.TwentySeventeen.Eight where

import Advent.Library
import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro.Platform

import Control.Monad.State

type Register = String

register :: Parser Register
register = some letterChar

delta :: Parser Int
delta = (id <$ string "inc" <|> negate <$ string "dec") <* char ' ' <*> integer

data Comp = Lt | Gt | Le | Ge | Eq | Neq deriving Show

comp :: Parser Comp
comp = try (Le <$ string "<=")
   <|> Lt <$ char '<'
   <|> try (Ge <$ string ">=")
   <|> Gt <$ char '>'
   <|> Neq <$ string "!="
   <|> Eq <$ string "=="

interpretComp :: Comp -> (Int -> Int -> Bool)
interpretComp c = case c of
  Lt -> (<)
  Gt -> (>)
  Le -> (<=)
  Ge -> (>=)
  Eq -> (==)
  Neq -> (/=)

type Expr = (Register, Comp, Int)

expr :: Parser Expr
expr = (,,) <$> register <* char ' ' <*> comp <* char ' ' <*> integer

type Clause = (Register, Int, Expr)

clause :: Parser Clause
clause = (,,) <$> register <* char ' ' <*> delta <* string " if " <*> expr

data ProgramState = ProgramState {
  _maxSeen :: Int,
  _registers :: Map Register Int
}
makeLenses ''ProgramState

initialState :: ProgramState
initialState = ProgramState 0 Map.empty

runProgram :: [Clause] -> ProgramState
runProgram = flip execState initialState . sequence_ . fmap interpretClause
  where
    interpretRegister :: Register -> State ProgramState Int
    interpretRegister r = Map.findWithDefault 0 r <$> use registers

    interpretCondition :: Expr -> State ProgramState Bool
    interpretCondition (r, c, n) = do
      v <- interpretRegister r
      let (.+.) = interpretComp c
      return $ v .+. n

    interpretClause :: Clause -> State ProgramState ()
    interpretClause (r, d, e) = do
      interpretCondition e >>= \case
        False -> return ()
        True -> applyDelta r d

    applyDelta :: Register -> Int -> State ProgramState ()
    applyDelta r d = do
      mold <- use $ registers . at r
      oldVal <- case mold of
        Nothing -> return 0
        Just v -> return v
      registers . at r ?= oldVal + d
      maxSeen %= max (oldVal + d)

main :: IO ()
main = defaultMain "2017.8" (pLines clause) $  \program -> do
  let finalState = runProgram program
  print . maximum . Map.elems $ finalState ^. registers
  print $ finalState ^. maxSeen