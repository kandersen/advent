{-# LANGUAGE OverloadedStrings #-}

module Advent.TwentyFifteen.Four where

import Advent.Library
import qualified Data.ByteString.Lazy.Char8 as BS

startsWithZeroes :: Int -> String -> Bool
startsWithZeroes n = (replicate n '0' ==) . take n

candidates :: String -> [String]
candidates secret = md5hash . (secret++) . show <$> [1..]

main :: IO ()
main = defaultMain "2015.4" (many anyChar) $ \secret -> do
  let (first, rest) = break (startsWithZeroes 5) (candidates secret)
  print $ length first + 1
  let (second, _) = break (startsWithZeroes 6) rest
  print $ length first + length second + 1