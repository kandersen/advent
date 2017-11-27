{-# LANGUAGE RankNTypes #-}
module Eight(main) where

import Advent
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.Reader
import Control.Monad.ST

data Command = RotateRow Int Int
             | RotateCol Int Int
             | Rect Int Int


pRotateRow, pRotateCol, pRect :: Parser Command
pRotateRow = RotateRow <$ string "rotate row y=" <* space <*> natural <* string " by " <*> natural
pRotateCol = RotateCol <$ string "rotate column x=" <* space <*> natural <* string " by " <*> natural
pRect = Rect <$ string "rect " <*> natural <* char 'x' <*> natural

pCommand :: Parser Command
pCommand = try pRotateRow <|> try pRotateCol <|> pRect

type Display = UArray (Int,Int) Bool

dWidth, dHeight :: Int
dWidth = 50
dHeight = 6

execute :: [Command] -> Display
execute cs = runSTUArray start
  where
    start :: ST s (STUArray s (Int,Int) Bool)
    start = do
      arr <- newArray ((0,0),(dWidth - 1,dHeight - 1)) False
      mapM_ (interpret arr) cs
      return arr

    interpret :: STUArray s (Int,Int) Bool -> Command -> ST s ()
    interpret arr (Rect w h) =
      forM_ (range ((0,0),(w - 1,h - 1))) $ \i ->
        writeArray arr i True
    interpret arr (RotateRow y n) = do
      replicateM_ n $ do
        end <- readArray arr (dWidth - 1,y)
        forM_ [dWidth - 2, dWidth - 3 .. 0] $ \x -> 
          readArray arr (x,y) >>= writeArray arr (x + 1, y)
        writeArray arr (0,y) end
    interpret arr (RotateCol x n) =
      replicateM_ n $ do
        end <- readArray arr (x,dHeight - 1)
        forM_ [dHeight - 2, dHeight - 3 .. 0] $ \y -> 
          readArray arr (x,y) >>= writeArray arr (x, y + 1)
        writeArray arr (x,0) end

litPixels :: Display -> Int
litPixels = length . filter id . elems 

display :: Display -> IO ()
display d = do
  let ((x0,y0),(w,h)) = bounds d
  forM_ [y0 .. h] $ \y -> do
    forM_ [x0 .. w] $ \x -> do
      putChar $ if (d ! (x,y)) then '#' else '.'
    putStrLn ""
  putStrLn ""

main :: IO ()
main = do
  putStrLn "----[ Part  I ]----"
  end <- execute . parseLines pCommand <$> getContents
  print $  litPixels $ end
  putStrLn "----[ Part II ]----"
  display end

