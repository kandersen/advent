{-# LANGUAGE OverloadedStrings #-}

module Four where

import Advent
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Digest.Pure.MD5 (md5)

hash :: String -> String
hash = show . md5 . BS.pack

isValid :: String -> Bool
isValid = ("000000" ==) . take 6

secret :: String
secret = "yzbqklnj"

candidates :: [String]
candidates = map ((secret++) . show) [1..]

main :: IO ()
main = print . (+1) . length . takeWhile (not . isValid) $ map hash candidates
