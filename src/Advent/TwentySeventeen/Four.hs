module Advent.TwentySeventeen.Four where

import Data.List (permutations)

import Advent.Library
import Advent.Library.Trie

validPassphrase1 :: [String] -> Bool
validPassphrase1 = go (leaf False)
  where 
    go :: Trie Char Bool -> [String] -> Bool
    go _ [] = True
    go t (w:ws) = insert'' w t False (\t' -> go t' ws)

validPassphrase2 :: [String] -> Bool
validPassphrase2 = go 0 (leaf Nothing)
  where
    go :: Int -> Trie Char (Maybe Int) -> [String] -> Bool
    go _ _ [] = True
    go n t (w:ws) = goPerms t (permutations w) (\t' -> go (n + 1) t' ws)
      where
        goPerms trie [] sk = sk trie
        goPerms trie (p:ps) sk = insert''' p n trie False (\t' -> goPerms t' ps sk)

main :: IO ()
main = defaultMain "2017.4" (pLines $ some letterChar `sepBy` char ' ') $ \input -> do
  print . length . filter validPassphrase1 $ input
  print . length . filter validPassphrase2 $ input