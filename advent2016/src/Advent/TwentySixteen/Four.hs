module Advent.TwentySixteen.Four(main) where

import Advent.Library
import Data.List (sortBy, sort, group, isInfixOf)
import Data.Ord (comparing, Down(..))

type Room = ([String], Int, String)

computeChecksum :: [String] -> String
computeChecksum =
  map head .take 5 . sortBy (comparing (Down . length)) . group . sort . concat 

isReal :: Room -> Bool
isReal (name,_,cs) = computeChecksum name == cs

roomID :: Room -> Int
roomID (_,id,_) = id

rotate :: Char -> Char
rotate 'z' = 'a'
rotate ' ' = ' '
rotate   c = succ c

rotateBy :: Int -> Char -> Char
rotateBy n c = iterate rotate c !! n 

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
main = defaultMain "2016.4" (pLines pRoom) $ \rs -> do
  putStrLn "-----[ Part I ]-----"
  putStr "Sum of real room IDs: "
  print . sum . map roomID . filter isReal $ rs
  putStrLn "-----[ Part II ]-----"
  putStr "North pole object storage room: "
  print $ roomID . head . filter (("northpole" `isInfixOf`) . decodeName) $ rs
