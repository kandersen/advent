{-# LANGUAGE OverloadedStrings #-}
module Twelve where

import           Advent
import           Data.Aeson
import           Data.Aeson.Parser
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import           Data.HashMap.Strict
import           Data.Monoid
import           Data.Scientific

isRedObject :: Object -> Bool
isRedObject = Prelude.any (== (String "red")) . elems

count :: (Foldable f, Functor f) => f Value -> Integer
count = getSum . fold . fmap (Sum . countNumbers)

countNumbers :: Value -> Integer
countNumbers val = case val  of
  Object o -> if isRedObject o
                 then 0
                 else count o
  Array a -> count a
  String _ -> 0
  Number n -> either (const (-100000000)) id (floatingOrInteger n)
  Bool _ -> 0
  Null -> 0

main :: IO ()
main = defaultMain $ print . countNumbers . unsafeParse

unsafeParse :: String -> Value
unsafeParse = either error id . AP.parseOnly json . BS.pack
