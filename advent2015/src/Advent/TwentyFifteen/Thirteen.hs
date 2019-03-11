module Advent.TwentyFifteen.Thirteen where

import Advent.Library

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (nub, maximum)

seatAt :: a -> [a] -> [[a]]
seatAt x [] = []
seatAt x (a:bs) = (a:x:bs) : map (a:) (seatAt x bs)

seatings :: [a] -> [[a]]
seatings [] = [[]]
seatings [x] = [[x]]
seatings [x,y] = [[x,y]]
seatings [x,y,z] = [[x,y,z]]
seatings (x:xs) = seatings xs >>= seatAt x

evalSeating :: (a -> a -> Int) -> [a] -> Int
evalSeating pf [] = 0
evalSeating pf xs@(first:_) = go xs
  where
    go [] = 0
    go [last] = pf first last + pf last first
    go (x:y:xs) = pf y x + pf x y + go (y:xs)

type Person = String

pPerson :: Parser Person
pPerson = some letterChar

type Relations = Map (Person, Person) Int

pMod :: Parser (Int -> Int)
pMod = id <$ string " would gain "
   <|> negate <$ string " would lose "

pRelation :: Parser (Relations -> Relations)
pRelation = (\a mod score b -> Map.insert (a, b) (mod score)) <$>
  pPerson <*> pMod <*> integer <* 
  string " happiness units by sitting next to " <*> pPerson <* char '.'

getGuests :: Relations -> [String]
getGuests = nub . fmap fst . Map.keys

main :: IO ()
main = defaultMain "2015.13" (pLines pRelation) $ \input -> do
  let relations = fromBuilders Map.empty input
  let guests = getGuests relations
  let pf1 a b = relations Map.! (a,b)
  print $ maximum . map (evalSeating pf1) $ seatings guests
  let pf "me" _ = 0
      pf _ "me" = 0
      pf a b    = relations Map.! (a,b)
  print $ maximum . map (evalSeating pf) $ seatings ("me" : guests)