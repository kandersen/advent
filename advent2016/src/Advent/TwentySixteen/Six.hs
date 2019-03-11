module Advent.TwentySixteen.Six(main) where

import Data.List (sort, group, maximumBy, minimumBy)
import Data.Ord (comparing)

freq :: ([String] -> String) -> String -> Char
freq f = head . f . group . sort

decode :: ([String] -> String) -> [String] -> String
decode f cs | all null cs = []
            | otherwise =
              let (chars, rest) = unzip [ (c,cs') | (c:cs') <- cs ] in
                freq f chars : decode f rest

main :: IO ()
main = do
  raw <- lines <$> getContents
  putStrLn "-----[ Part  I ]-----"
  putStrLn . decode (maximumBy $ comparing length) $ raw
  putStrLn "-----[ Part II ]-----"
  putStrLn . decode (minimumBy $ comparing length) $ raw