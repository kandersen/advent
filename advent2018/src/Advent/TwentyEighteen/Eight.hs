module Advent.TwentyEighteen.Eight where

import Advent.Library
import Control.Monad
import Data.Maybe

data Tree = Node [Int] [Tree]

pTree :: Parser Tree
pTree = do
    nchildren <- natural
    space
    nmeta <- natural
    space
    children <- replicateM nchildren pTree 
    space
    meta <- replicateM nmeta (natural <* space)
    return $ Node meta children

sumMeta :: Tree -> Int
sumMeta (Node ms cs) = sum ms + sum (sumMeta <$> cs)

value :: Tree -> Int
value (Node ms []) = sum ms
value (Node ms cs) = sum $ maybe 0 value . (cs !!?) . pred <$> ms

main :: IO ()
main = defaultMain "2018.8" pTree $ \t -> do
    print . sumMeta $ t
    print . value $ t