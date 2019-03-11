module Advent.TwentySixteen.Three where

type Spec = (Int, Int, Int)

isPossible :: Spec -> Bool
isPossible (a, b, c) = a + b > c && b + c > a && a + c > b

------------------------------------------------------------------------------

parseSpec :: [Int] -> Spec
parseSpec s = case s of
  [a,b,c] -> (a, b, c)

------------------------------------------------------------------------------

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go
  where
    go xs = case splitAt n xs of
      ([],b) -> []
      (a, b) -> a : go b

parseVertSpecs :: [[Int]] -> [Spec]
parseVertSpecs xs = case xs of
  [[a1,a2,a3],
   [b1,b2,b3],
   [c1,c2,c3]] -> [(a1, b1, c1), (a2,b2,c2), (a3,b3,c3)]

------------------------------------------------------------------------------

main :: IO ()
main = do
  rawInput <- map (map read . words) . lines <$> getContents
  putStrLn "---[ Part  I ]---"
  putStr "Valid triangles: "
  print . length . filter isPossible $ map parseSpec rawInput
  putStrLn "---[ Part II ]---"
  let specs = concatMap parseVertSpecs . chunksOf 3 $ rawInput
  print . length . filter isPossible $ specs
