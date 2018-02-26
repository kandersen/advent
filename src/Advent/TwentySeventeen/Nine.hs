{-# LANGUAGE Strict #-}
module Advent.TwentySeventeen.Nine where

import Advent.Library

main :: IO ()
main = defaultMain "2017.9" (many asciiChar) $ \stream -> do
  print . scoreGroups $ stream
  print . countGarbage $ stream


scoreGroups :: [Char] -> Int
scoreGroups = outGarbage 0 0
  where
    outGarbage _ acc [] = acc
    outGarbage depth acc ('{':s) = outGarbage (depth + 1) acc s
    outGarbage depth acc ('}':s) = outGarbage (depth - 1) (acc + depth) s
    outGarbage depth acc (',':s) = outGarbage depth acc s
    outGarbage depth acc ('<':s) = inGarbage depth acc s
 
    inGarbage depth acc ('!':_:s) = inGarbage depth acc s
    inGarbage depth acc ('>':s) = outGarbage depth acc s
    inGarbage depth acc (_:s) = inGarbage depth acc s

countGarbage :: [Char] -> Int
countGarbage = outGarbage 0
  where
    outGarbage acc [] = acc
    outGarbage acc ('<':s) = inGarbage acc s
    outGarbage acc (_:s) = outGarbage acc s

    inGarbage acc ('!':_:s) = inGarbage acc s
    inGarbage acc ('>':s) = outGarbage acc s
    inGarbage acc (_:s) = inGarbage (acc + 1) s