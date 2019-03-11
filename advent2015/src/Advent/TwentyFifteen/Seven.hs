module Advent.TwentyFifteen.Seven where
import           Advent.Library
import           Control.Monad.Identity
import           Control.Monad.State
import Data.Bits (
  (.&.), --and
  (.|.), --or -
  complement,
  shiftL,
  shiftR)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word (Word16)
import           Lens.Micro.Platform

type Val = Either String Word16

pVar :: Parser String
pVar = some lowerChar

pConst :: Parser Word16
pConst = fromIntegral <$> natural

pVal :: Parser Val
pVal = Right <$> pConst
   <|> Left <$> pVar

data Expr = AND Val Val
          | OR Val Val
          | NOT Val
          | LSHIFT Val Int
          | RSHIFT Val Int
          | VAR String
          | CONST Word16
          deriving Show

pBinOp :: (a -> b -> Expr) -> String -> Parser a -> Parser b -> Parser Expr
pBinOp sem op l r = sem <$> l <* space <* string op <* space <*> r

pUnOp :: (a -> Expr) -> String -> Parser a -> Parser Expr
pUnOp sem op arg = sem <$ string op <* space <*> arg

pExpr :: Parser Expr
pExpr = try (pBinOp AND "AND" pVal pVal)
    <|> try (pBinOp OR "OR" pVal pVal)
    <|> try (pBinOp RSHIFT "RSHIFT" pVal natural)
    <|> try (pBinOp LSHIFT "LSHIFT" pVal natural)
    <|> pUnOp NOT "NOT" pVal
    <|> CONST <$> pConst
    <|> VAR <$> pVar

pRule :: Parser (Rules -> Rules)
pRule = flip Map.insert <$> pExpr <* space <* string "->" <* space <*> pVar

type Rules = Map String Expr

type Evaluator a = State Rules a

computeAndCache :: String -> Evaluator Word16 -> Evaluator Word16
computeAndCache wire m = do
  val <- m
  at wire ?= CONST val
  return val

evalVal :: Val -> Evaluator Word16
evalVal (Left x) = evalWire x
evalVal (Right w) = return w

evalWire :: String -> Evaluator Word16
evalWire wire = do
  mrule <- use (at wire)
  case mrule of
    Nothing -> error $ "no rule for wire: " ++ wire
    Just rule ->
      computeAndCache wire $ case rule of
        AND x y    -> (.&.) <$> evalVal x <*> evalVal y
        OR x y     -> (.|.) <$> evalVal x <*> evalVal y
        NOT x      -> complement <$> evalVal x
        LSHIFT x n -> flip shiftL n <$> evalVal x
        RSHIFT x n -> flip shiftR n <$> evalVal x
        VAR x      -> evalWire x
        CONST w    -> return w

evalCircuit :: Evaluator ()
evalCircuit = do
  wires <- Map.keys <$> get
  forM_ wires evalWire

main :: IO ()
main = defaultMain "2015.7" (fromBuilders Map.empty <$> pLines pRule) $ \rules -> do
  let initA = evalState (evalWire "a") rules
  print initA
  print $ evalState (evalWire "a") (Map.insert "b" (CONST initA) rules)
