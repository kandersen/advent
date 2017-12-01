-- |

module Five where
import Advent

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

subsequencesOf2 :: [a] -> [(a,a)]
subsequencesOf2 [] = []
subsequencesOf2 [x] = []
subsequencesOf2 (x:y:as) = (x, y) : subsequencesOf2 (y:as)

has3Vowels = (3 <=) . length . filter isVowel
hasRepeat = any (\(a, b) -> a == b) . subsequencesOf2
noBadPairs = not . any (`elem` badPairs) . subsequencesOf2
  where
    badPairs = [('a','b'),('c','d'),('p','q'),('x','y')]

isNice :: String -> Bool
isNice s = has3Vowels s && hasRepeat s && noBadPairs s

subsequencesOf3 :: [a] -> [(a,a,a)]
subsequencesOf3 (x:y:z:as) = (x,y,z) : subsequencesOf3 (y:z:as)
subsequencesOf3 _ = []

repeatingSubsequence (x:y:as) = any (== (x,y)) (subsequencesOf2 as) || repeatingSubsequence (y:as)
repeatingSubsequence _ = False

hasRepeatWithLetterBetween = any (\(a,_,b) -> a == b) . subsequencesOf3

isNicer :: String -> Bool
isNicer = (&&) <$> hasRepeatWithLetterBetween <*> repeatingSubsequence

main :: IO ()
main = defaultMain $ \input -> do
  print . length . filter isNice $ lines input
  print . length . filter isNicer $ lines input
