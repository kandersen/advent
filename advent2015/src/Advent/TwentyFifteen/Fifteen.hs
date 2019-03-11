module Advent.TwentyFifteen.Fifteen where

import Advent.Library
import Data.Map

type Ingredient = [Int]

pIngredient :: Parser Ingredient
pIngredient = (\a b c d e -> [a, b, c, d, e]) <$
  some letterChar <* string ": capacity " <*> integer 
  <* string ", durability " <*> integer
  <* string ", flavor " <*> integer
  <* string ", texture " <*> integer
  <* string ", calories " <*> integer

type Selection = Map Ingredient Int

main :: IO ()
main = defaultMain "2015.15" (pLines pIngredient) print