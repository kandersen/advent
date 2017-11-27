{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Advent(
  module Advent,
  Parser
  ) where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Data.Char (digitToInt)

import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Sequence as Seq
import Control.Monad.State

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

natural :: (Num a) => Parser a
natural = foldl (\acc n -> acc * 10 + fromIntegral (digitToInt n)) 0 <$> some digitChar

integer :: (Read a, Num a) => Parser a
integer = (char '-' *> (negate <$> integer))
      <|> natural

parseLines :: Parser a -> String -> [a]
parseLines p s = case runParser (manyTill (p <* eol) eof) "Advent: parseLines" s of
  Left e -> error $ parseErrorPretty e
  Right as -> as

md5hash :: String -> String
md5hash = show . md5 . BS.pack

pop :: (MonadState (Seq a) m) => m (Maybe a)
pop = do
  Seq.viewl <$> get >>= \case
    EmptyL -> return Nothing
    a :< q' -> do
      put q'
      return $ Just a

push :: (MonadState (Seq a) m) => a -> m ()
push a = modify ((|> a))
