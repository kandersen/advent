{-# LANGUAGE LambdaCase #-}
module Advent.TwentySixteen.Twentyone (main) where

import Advent.Library
import Text.Megaparsec hiding (State)

import Control.Monad.State
import Control.Monad.Identity
import Lens.Micro.Platform
import Data.Array.IArray


--------------------------------------------------------------------------------

data Command = Swap Int Int
             | Replace Char Char
             | Rotate Int
             | RotateBasedOn Char
             | Reverse Int Int
             | Move Int Int
             deriving Show

----[ Parsing ]-----------------------------------------------------------------

parseSwap :: Parser Command
parseSwap = Swap <$ string "swap position " <*> integer <* string " with position " <*> integer

parseReplace :: Parser Command
parseReplace = Replace <$ string "swap letter " <*> lowerChar <* string " with letter " <*> lowerChar

parseRotate :: Parser Command
parseRotate = Rotate <$ string "rotate " <*> (left <|> right) <* string " step" <* optional (char 's') 
  where
    left, right :: Parser Int
    left = negate <$ string "left " <*> integer
    right = string "right " *> integer

parseRotateBasedOn :: Parser Command
parseRotateBasedOn = RotateBasedOn <$ string "rotate based on position of letter " <*> lowerChar

parseReverse :: Parser Command
parseReverse = Reverse <$ string "reverse positions " <*> integer <* string " through " <*> integer

parseMove :: Parser Command
parseMove = Move <$ string "move position " <*> integer <* string " to position " <*> integer

parseCommand :: Parser Command
parseCommand = try parseSwap
               <|> try parseReplace
               <|> try parseRotate
               <|> try parseRotateBasedOn
               <|> try parseReverse
               <|> parseMove

--------------------------------------------------------------------------------

runProgram :: [Command] -> String
runProgram = elems . flip execState (listArray ixs initString) . mapM_ exec
  where
    initString = "abcdefgh"
    ixs :: (Int,Int)
    ixs = (0, length initString - 1)
    
    exec :: Command -> State (Array Int Char) ()
    exec (Swap a b) = do
      aval <- (! a) <$> get
      bval <- (! b) <$> get
      ix a .= bval
      ix b .= aval
    exec (Replace c d) = 
      modify $ amap $ \case
        x | x == c -> d
          | x == d -> c
          | otherwise -> x
    exec (Rotate n) = 
      modify $ ixmap ixs $ \i -> (i + n) `mod` length initString      
    exec (RotateBasedOn c) = return ()
      -- n <- _ $ 
      --   forM_ ixs $ \i -> do
      --     val <- lift $ (! i) <$> get
      --     if (val == c) then undefined else undefined
      -- modify $ ixmap ixs $ \i -> (i + n) `mod` length initString
    exec (Reverse _ _) = return ()
    exec (Move _ _) = return ()

main :: IO ()
main = defaultMain "2016-21" (pLines parseCommand) $ \prog -> do
  print prog
  print $ runProgram prog