module Four(main) where

import Advent
import Data.List (sortBy, sort, group, isInfixOf)
import Data.Ord (comparing, Down(..))

import Text.Megaparsec
import Text.Megaparsec.String

type Room = ([String], Int, String)

computeChecksum :: [String] -> String
computeChecksum name =
  map head .take 5 . sortBy (comparing (Down . length)) . group . sort . concat $ name

isReal :: Room -> Bool
isReal (name,_,cs) = computeChecksum name == cs

roomID :: Room -> Int
roomID (_,id,_) = id

rotate :: Char -> Char
rotate 'z' = 'a'
rotate ' ' = ' '
rotate   c = succ c

rotateBy :: Int -> Char -> Char
rotateBy n c = (iterate rotate c) !! n 

decodeName :: Room -> String
decodeName (name,id,_) = unwords $ map (map $ rotateBy id) name

--------------------------------------------------------------------------------

pChecksum :: Parser String
pChecksum = brackets $ count 5 lowerChar

pRoom :: Parser Room
pRoom = do
  name <- many (many lowerChar <* char '-')
  id <- read <$> many digitChar
  cs <- pChecksum
  return (name, id, cs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  rs <- parseLines pRoom <$> getContents
  putStrLn $ "-----[ Part I ]-----"
  putStr "Sum of real room IDs: "
  print . sum . map roomID . filter isReal $ rs
  putStrLn $ "-----[ Part II ]-----"
  putStr "North pole object storage room: "
  print $ roomID . head . filter (("northpole" `isInfixOf`) . decodeName) $ rs
