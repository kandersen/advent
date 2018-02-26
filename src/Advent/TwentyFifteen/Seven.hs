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

pVal :: Parser Val
pVal = Right . fromIntegral <$> natural
   <|> Left <$> many lowerChar

data Expr = AND Val Val
          | OR Val Val
          | NOT Val
          | LSHIFT Val Int
          | RSHIFT Val Int
          | CONST Val

pBinOp :: (a -> b -> Expr) -> String -> Parser a -> Parser b -> Parser Expr
pBinOp sem op l r = sem <*> l <* space1 <* string op <* space1 <*> r

pUnOp :: (a -> Expr) -> String -> Parser a -> Parser Expr
pUnOp sem op arg = sem <* op <* space1 <*> arg

pExpr :: Parser Expr
pExpr = pBinOp AND "AND" pVal pVal 
    <|> pBinOp OR "OR" pVal pVal
    <|> pBinOp LSHIFT "LSHIFT" val natural
    <|> pBinOp RSHIFT "RSHIFT" val natural
    <|> pUnOp NOT "NOT" val
    <|> pUnOp CONST "CONST" val

type Rules = Map String Expr

pRule :: Parser (Rules -> Rules)
pRule = do
  e <- pExpr
          

parseRule :: String -> (Rules -> Rules)
parseRule s = case words s of
 ["1","AND",y,_,z] -> Map.insert z (AND (Right . fromIntegral $ 1) (Left y))
 [x,"AND",y,_,z] -> Map.insert z (AND (Left x) (Left y))
 [x,"OR",y,_,z] -> Map.insert z (OR (Left x) (Left y))
 ["NOT",x,_,z] -> Map.insert z (NOT (Left x))
 [x,"LSHIFT",n,_,z] -> Map.insert z (LSHIFT (Left x) $ read n)
 [x,"RSHIFT",n,_,z] -> Map.insert z (RSHIFT (Left x) $ read n)
 [x,_,z] -> case (reads :: ReadS Word16) x of
   [] -> Map.insert z (CONST . Left $ x)
   _ -> Map.insert z (CONST . Right $ read x)
 _ -> error $ "malformed input: " ++ s



pRule :: Parser (Rules -> Rules)
pRule =  undefined

type Evaluator m a = StateT Rules m a

computeAndCache :: Monad m => String -> Evaluator m Word16 -> Evaluator m Word16
computeAndCache wire m = do
  val <- m
  at wire .= (Just . CONST . Right) val
  return val

evalVar :: (Functor m, Monad m) => Val -> Evaluator m Word16
evalVar (Left x) = evalWire x
evalVar (Right w) = return w

evalWire :: (Functor m, Monad m) => String -> Evaluator m Word16
evalWire wire = do
  mrule <- use (at wire)
  case mrule of
    Nothing -> error $ "no rule for wire: " ++ wire
    Just rule ->
      case rule of
        AND x y -> computeAndCache wire ((.&.) <$> evalVar x <*> evalVar y)
        OR x y -> computeAndCache wire ((.|.) <$> evalVar x <*> evalVar y)
        NOT x -> computeAndCache wire (complement <$> evalVar x)
        LSHIFT x n -> computeAndCache wire (flip shiftL n <$> evalVar x)
        RSHIFT x n -> computeAndCache wire (flip shiftL n <$> evalVar x)
        CONST (Left x) -> computeAndCache wire (evalVar (Left x))
        CONST (Right x) -> return x

main :: IO ()
main = defaultMain "2015.7" (return [id]) $ \input -> do
  let rules = foldl (flip ($)) Map.empty $ input
  print $ runIdentity $ evalStateT (evalWire "a") rules
