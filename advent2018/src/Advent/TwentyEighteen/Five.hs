module Advent.TwentyEighteen.Five where

import Advent.Library hiding (match)
import Data.Char (ord, toLower)

match :: Char -> Char -> Bool
match x y = abs (ord x - ord y) == 32

react :: String -> String
react = go
  where
    go :: String -> String
    go [] = []
    go [x] = [x]
    go (x:xs) = case go xs of
        [] -> [x]
        r@(y:ys) | match x y -> ys
                 | otherwise -> x : r

reduce :: Char -> String -> String
reduce c = filter ((c /=) . toLower)

optimize :: String -> Int
optimize = minimum . zipWith (\c -> length . react . reduce c) ['a'..'z'] . repeat

main :: IO ()
main = defaultMain "2018.5" (many letterChar) $ \polymer -> do
    print $ length polymer
    print $ length . react $ polymer
    print $ optimize polymer