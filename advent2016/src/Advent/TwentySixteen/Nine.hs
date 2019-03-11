module Advent.TwentySixteen.Nine(main) where

import Advent.Library
import Data.List.NonEmpty (NonEmpty(..))
import Text.Megaparsec

type Marker = (Int,Int)

pMarker :: Parser Marker
pMarker = parens ((,) <$> natural <* char 'x' <*> natural)

parseMarker :: String -> (Marker, String)
parseMarker s = case runParser' pMarker pstate of
  (pstate', Right m) -> (m, stateInput pstate')
  _ -> error "parseError"
  where
    pstate = State s (SourcePos "" (mkPos 1) (mkPos 1) :| []) 0 (mkPos 1)

decompressedLength :: String -> Int
decompressedLength = go 0
  where
    go :: Int -> String -> Int
    go acc s@('(':_) = let ((n,x), s') = parseMarker s in
      go (acc + n * x) (drop n s')
    go acc (_:cs') = go (acc + 1) cs'
    go acc [] = acc

decompressedLength' :: String -> Int
decompressedLength' = go 0
  where
    go :: Int -> String -> Int
    go acc s@('(':_) =
      let ((n,x), s') = parseMarker s in
      let (hd,tl) = splitAt n s' in
        go (acc + decompressedLength' hd * x) tl
    go acc (_:cs') = go (acc + 1) cs'
    go acc [] = acc


main :: IO ()
main = do
  raw <- getContents
  putStrLn "----[ Part  I ]----"  
  print $ decompressedLength raw
  putStrLn "----[ Part II ]----"
  print $ decompressedLength' raw
