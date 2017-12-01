module Ten where

import Advent
import Data.List

input :: String
input = "1113222113"

say :: String -> String
say s@(x:_) = show (length s) ++ [x]

lookAndSay :: String -> String
lookAndSay = concat . map say . group

main = print . length . (!!50) $ iterate lookAndSay input
