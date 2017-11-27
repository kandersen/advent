module Seven where
import           Advent
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
import           Lens.Micro
import           Lens.Micro.GHC
import           Lens.Micro.Mtl

type Val = Either String Word16

data Rule = AND Val Val
          | OR Val Val
          | NOT Val
          | LSHIFT Val Int
          | RSHIFT Val Int
          | CONST Val

type Rules = Map String Rule

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
main = defaultMain $ \input -> do
  let rules = foldl (flip ($)) Map.empty . map parseRule . lines $ input
  print $ runIdentity $ evalStateT (evalWire "a") rules
