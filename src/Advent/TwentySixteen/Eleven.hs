module Eleven(main) where

import Advent
import Data.Bits
import Data.Word
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Data.List ((\\))
{-

State layout - Each box is two bits indicating floor, 1-4, of the respective
[G]enerator and [M]microchip.

* [T]hulium
* [Pl]utonium
* [S]trontium
* [Pr]omethium
* [R]uthenium

Minimum 22 bits required.

#             11  10    9    8     7    6     5    4     3     2    1
  +---------+---+----+----+-----+-----+----+----+-----+-----+----+----+
  | 10 Bits | E | TG | TM | PlG | PlM | SG | SM | PrG | PrM | RG | RM |
  +---------+---+----+----+-----+-----+----+----+-----+-----+----+----+

-}

itemAssignment :: [(String,Int)]
itemAssignment = [
  ("ruthenium" , 1),
  ("promethium", 3),
  ("strontium" , 5),
  ("plutonium" , 7),
  ("thulium"   , 9) ]

----[ Parsing ]-----------------------------------------------------------------

pGenOrChip :: Int -> Parser Int
pGenOrChip n = string " generator" *> pure (n + 1)
           <|> string "-compatible microchip" *> pure n

pItem :: Parser Int
pItem = choice $ flip map itemAssignment $ \(elem,n) ->
  string elem *> pGenOrChip n

pNumber :: Parser Int
pNumber = string "first " *> pure 0
          <|> string "second " *> pure 1
          <|> string "third " *> pure 2
          <|> string "fourth " *> pure 3

pFloor :: Parser Int
pFloor = string "The " *> pNumber <* string "floor "

pItems :: Parser [Int]
pItems = some ((string "a " *> pItem) <* (string " and " <|> try (string ", and ") <|> try (string ", ") <|> string "."))
         <|> string "nothing relevant." *> pure []

pLine :: Parser Word32
pLine = buildFloor <$> pFloor <* string "contains " <*> pItems

parseInput :: String -> Word32
parseInput = (.|. elevatorAt 0) . combineStates . parseLines pLine

--------------------------------------------------------------------------------

type State = Word32

itemAt :: Int -> Int -> Word32
itemAt f i = (toEnum f) `shiftL` (2 * (i - 1))

buildFloor :: Int -> [Int] -> Word32
buildFloor f = combineStates . map (itemAt f)

combineStates :: [Word32] -> Word32
combineStates = foldr (.|.) (toEnum 0)

elevatorAt :: Int -> Word32
elevatorAt floor = itemAt floor 11

floorOfItem :: Word32 -> Int -> Int
floorOfItem s i = fromEnum $ (s `shiftR` (2 * (i - 1))) .&. toEnum 3

elevatorFloor :: Word32 -> Int
elevatorFloor = flip floorOfItem 11

itemsOnFloor :: Word32 -> Int -> [Int]
itemsOnFloor s f = filter ((== f) . floorOfItem s)  [1..10]

subs1 :: [a] -> [[a]]
subs1 = map (\a -> [a])

subs2 :: Eq a => [a] -> [[a]]
subs2 xs = do
  a <- xs
  b <- xs \\ [a]
  return $ [a, b]

subs1or2 :: Eq a => [a] -> [[a]]
subs1or2 = (++) <$> subs1 <*> subs2

moveItemUp :: State -> Int -> Maybe State
moveItemUp s i = moveItem s 1 i

moveItemDown :: State -> Int -> Maybe State
moveItemDown s i = moveItem s (-1) i

moveItem :: State -> Int -> Int -> Maybe State
moveItem s d i = do
  let f = floorOfItem s i
  if 0<= f + d && f + d <= 3
    then return $ (itemAt f i `xor` s) .|. itemAt (f + d) i
    else Nothing

moveItems :: (State -> Int -> Maybe State) -> State -> [Int] -> Maybe State
moveItems move s [] = return s
moveItems move s (i:is) = do
  s' <- move s i
  undefined --move s' is



successors :: State -> [State]
successors s = do
  candidateMoves <- subs1or2 . itemsOnFloor s $ elevatorFloor s
  
  undefined

--------------------------------------------------------------------------------

main :: IO ()
main = do
  initialState <- parseInput <$> getContents
  print initialState
