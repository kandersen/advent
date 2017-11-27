module Advent (
  defaultMain,
  splitOn,
  isSubsequenceOf,
  nub,
  maximumBy,
  on,
  module Control.Applicative,
) where

import           Control.Applicative
import           Data.List (isPrefixOf, nub, maximumBy)
import Data.Function (on)
import           System.Environment (getArgs)

defaultMain :: (String -> IO ()) -> IO ()
defaultMain body = do
  [inputFile] <- getArgs
  body =<< readFile inputFile

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c input = case break (==c) input of
  (xs, []) -> [xs]
  (xs, _:ys) -> xs : splitOn c ys

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []        _ = True
isSubsequenceOf  _       [] = False
isSubsequenceOf xs ys@(_:ys') =
  xs `isPrefixOf` ys || xs `isSubsequenceOf` ys'
