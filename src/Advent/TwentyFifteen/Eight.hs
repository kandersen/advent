module Eight where

import Advent

code :: String -> Integer
code = fromIntegral . length

foo :: String -> Integer
foo = go
  where
    go [] = 2
    go ('\"':xs) = 2 + go xs
    go ('\\':xs) = 2 + go xs
    go (_:xs) = 1 + go xs

memory :: String -> Integer
memory = go . tail
  where
    go "\"" = 0
    go ('\\':'\\':xs) = 1 + go xs
    go ('\\':'\"':xs) = 1 + go xs
    go ('\\':'x':a:b:xs) = 1 + go xs
    go (_:xs) = 1 + go xs

encode :: String -> String
encode xs = "\"\\\"" ++ (go . init . tail $ xs) ++ "\\\"\""
  where
    go ('\\':'x':a:b:xs) = '\\':'\\':'x':a:b: go xs
    go ('\\':'\"':xs) = '\\':'\\':'\\':'\"': go xs
    go ('\\':xs) = '\\':'\\': go xs
    go (c:xs) = c : go xs
    go [] = []

main :: IO ()
main = defaultMain $ \input -> do
  print $ (sum . map code . lines $ input) - (sum . map memory . lines $ input)
  print $ (sum . map foo . lines  $ input) - (sum . map code . lines $ input)
