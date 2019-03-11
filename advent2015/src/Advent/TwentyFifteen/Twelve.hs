{-# LANGUAGE OverloadedStrings #-}
module Advent.TwentyFifteen.Twelve where

import           Advent.Library hiding (count, count')
import           Data.Aeson
import           Data.Aeson.Parser
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import           Data.HashMap.Strict
import           Data.Monoid
import           Data.Scientific

count :: (Foldable f, Functor f) => f Value -> Integer
count = getSum . fold . fmap (Sum . countNumbers)

countNumbers :: Value -> Integer
countNumbers val = case val  of
  Object o -> count o
  Array a -> count a
  String _ -> 0
  Number n -> either (const (-100000000)) id (floatingOrInteger n)
  Bool _ -> 0
  Null -> 0

isRedObject :: Object -> Bool
isRedObject = elem (String "red") . elems  

count' :: (Foldable f, Functor f) => f Value -> Integer
count' = getSum . fold . fmap (Sum . countNumbers')

countNumbers' :: Value -> Integer
countNumbers' val = case val  of
  Object o -> if isRedObject o
                 then 0
                 else count' o
  Array a -> count' a
  String _ -> 0
  Number n -> either (const (-100000000)) id (floatingOrInteger n)
  Bool _ -> 0
  Null -> 0  

unsafeParse :: String -> Value
unsafeParse = either error id . AP.parseOnly json . BS.pack
  
main :: IO ()
main = defaultMain "2015.12" (many anyChar) $ \raw -> do
  let input = unsafeParse raw
  print . countNumbers $ input
  print . countNumbers' $ input