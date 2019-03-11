module Advent.TwentySixteen.Fourteen(main) where

import Advent.Library

stream :: Int -> String -> [String]
stream n salt = (!! n) . iterate md5hash . (salt++) . show <$> ([0..] :: [Int])

keyIndices :: [String] -> [Int]
keyIndices = go 0
  where
    go :: Int -> [String] -> [Int]
    go index (hash:s) =
      if any (check 1000 s) (triplet hash)
      then index : go (index + 1) s
      else         go (index + 1) s

    check :: Int -> [String] -> Char -> Bool
    check 0        _ _ = False
    check n (hash:s) c = hasQuint c hash || check (n - 1) s c

triplet :: String -> Maybe Char
triplet (a:xs@(b:c:_)) | a == b && b == c = Just a
                       | otherwise        = triplet xs
triplet              _                    = Nothing

hasQuint :: Char -> String -> Bool
hasQuint x (a:xs@(b:c:d:e:_)) = (x == a && a == b && b == c && c == d && d == e) ||
                                hasQuint x xs
hasQuint _                  _ = False

main :: IO ()
main = do
  salt <- getLine
  putStrLn "----[ Part  I ]----"
  print . (!!63) . keyIndices . stream 1 $ salt
  putStrLn "----[ Part II ]----"
  print . (!!63) . keyIndices . stream 2017 $ salt
