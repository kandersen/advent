module Advent.TwentyFifteen.Ten where

import Advent.Library
import Data.List

say :: String -> String
say s@(x:_) = show (length s) ++ [x]

lookAndSay :: String -> String
lookAndSay = concatMap say . group

main = defaultMain "2015.10" (some digitChar) $ \input -> do
    let seqs = iterate lookAndSay input
    print $ length . (!!40) $ seqs
    print $ length . (!!50) $ seqs

    