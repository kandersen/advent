module Advent.TwentySixteen.Five(main) where

import Advent.Library
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State

interestingDigits :: String -> [(Char,Char)]
interestingDigits seed = go 0
  where
    go n = case md5hash (seed ++ show n) of
      ('0':'0':'0':'0':'0':i:c:_) -> (i,c) : go (n + 1)
      _ -> go (n + 1)

findHardPassword :: [(Char,Char)] -> String
findHardPassword stream = map ((IntMap.!) (execState (go stream) IntMap.empty)) [0..7]
  where
    go :: [(Char,Char)] -> State (IntMap Char) ()
    go ((i,c):s') = do
      case readPosition i of
        Nothing -> go s'
        Just i' -> do
          modify (IntMap.alter (putIfEmpty c) i')
          count <- IntMap.size <$> get
          if count == 8
            then return ()            else go s'

putIfEmpty :: Char -> (Maybe Char -> Maybe Char)
putIfEmpty c    Nothing = Just c
putIfEmpty _ v@(Just _) = v

readPosition :: Char -> Maybe Int
readPosition p | fromEnum '0' <= fromEnum p && fromEnum p <= fromEnum '7' =
                 Just $ fromEnum p - fromEnum '0'
               | otherwise =
                 Nothing

main :: IO ()
main = do
  seed <- getLine
  putStrLn "-----[ Part  I ]-----"
  putStrLn . take 8 . map fst $ interestingDigits seed
  putStrLn "-----[ Part II ]-----"
  putStrLn . findHardPassword $ interestingDigits seed
  
