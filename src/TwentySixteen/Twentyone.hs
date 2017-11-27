{-# LANGUAGE LambdaCase #-}
module Twentyone(main) where

import Advent
import Text.Megaparsec hiding (State)

import Control.Monad.State
import Control.Lens
import Data.Array.IArray
import Control.Monad.Trans.Either


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
    exec (Replace c d) = do
      modify $ amap $ \case
        x | x == c -> d
          | x == d -> c
          | True -> x
    exec (Rotate n) = do
      modify $ ixmap ixs $ \i -> (i + n) `mod` length initString      
    -- exec (RotateBasedOn c) = do
    --   n <- either id (const $ error "shouldn't happen") . runEitherT $ forM_ ixs $ \i -> do
    --     val <- (!i) <$> lift get
    --     if val == c
    --       then left i
    --       else return ()
    --   modify $ ixmap ixs $ \i -> (i + n) `mod` length initString
    exec (Reverse from to) = return ()
    exec (Move from to) = return ()

main :: IO ()
main = do
  prog <- parseLines parseCommand <$> getContents
  print prog
