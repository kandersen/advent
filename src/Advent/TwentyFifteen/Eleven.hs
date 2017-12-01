module Eleven where

import Advent
import Data.List

input :: String
input = "vzbxkghb"

nextChar :: Char -> (Char, Bool)
nextChar 'z' = ('a', True)
nextChar 'h' = ('j', False)
nextChar 'n' = ('p', False)
nextChar 'k' = ('m', False)
nextChar c = (succ c, False)

next :: String -> String
next = go True
  where
    go False    xs = xs
    go True (c:cs) = let (c', carry) = nextChar c in c' : go carry cs
    go True      _ = []

containsDecreasingTriple :: String -> Bool
containsDecreasingTriple (c:xs@(b:a:_)) =
  (c == succ b && b == succ a) || containsDecreasingTriple xs
containsDecreasingTriple _ = False

hasTwoPairsOfLetters :: String -> Bool
hasTwoPairsOfLetters = start
  where
    start (a:b:xs) = (a == b && foundFirst a xs) || start (b:xs)
    start        _ = False
    foundFirst c (a:xs) | c == a =
      foundFirst c xs
    foundFirst c (a:b:xs)        =
      (a == b && a /= c) || foundFirst c (b:xs)
    foundFirst _ _               =
      False

isValid :: String -> Bool
isValid = (&&) <$> containsDecreasingTriple <*> hasTwoPairsOfLetters

nextPassword :: String -> String
nextPassword = maybe "error" reverse . find isValid . iterate next . next . reverse

main :: IO ()
main = putStrLn . nextPassword . nextPassword $ input
