{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Ten(main) where

import Advent

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Either (partitionEithers)

import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Lens hiding (Empty)
import Control.Monad.Trans.Reader

import Text.Megaparsec hiding (State)
import Text.Megaparsec.String

--------------------------------------------------------------------------------

type BotID = Int
type OutID = Int

data BotState = Empty
              | One Int
              deriving Show

data Network = Network {
  _bots :: IntMap (BotState, [Int]),
  _outputs :: IntMap [Int]
  }
makeLenses ''Network

type Rules = IntMap (Either BotID OutID, Either BotID OutID)

runNetwork :: Rules -> [Input] -> Network
runNetwork rules = flip execState (Network IM.empty IM.empty) . mapM_ (uncurry give)
                       
    where 
      give :: Int -> (Either BotID OutID) -> State Network ()
      give a (Right outID) =
        outputs %= IM.alter newOrInsert outID
        where
          newOrInsert Nothing = Just [a]
          newOrInsert (Just ns) = Just (a:ns)
      give a (Left botID) = do
        mbs <- IM.lookup botID <$> use bots
        case mbs of
          Just (One b, hist) -> do
            bots %= IM.insert botID (Empty, a:b:hist)
            let (lowID, highID) = rules IM.! botID 
            give (min a b) lowID
            give (max a b) highID
          _ -> bots %= IM.insert botID (One a, [a])

type Input = (Int,Either BotID OutID)


------------------------------------------------------------------- -------------

type Bot = (Int, (Either BotID OutID, Either BotID OutID))

toRules :: [Bot] -> Rules
toRules = IM.fromList

pBot :: Parser BotID
pBot = string "bot " *> natural

pOutput :: Parser OutID
pOutput = string "output " *> natural

pBotOrOutput :: Parser (Either BotID OutID)
pBotOrOutput = eitherP (try pBot) pOutput

pValue :: Parser Int
pValue = string "value " *> natural

pInput :: Parser (Either Bot Input)
pInput = eitherP (try botDesc) inputDesc
  where
    botDesc = (\botID low high -> (botID, (low, high))) <$> pBot <* string " gives low to " <*> pBotOrOutput <* string " and high to " <*> pBotOrOutput
    inputDesc = (\n b -> (n, Left b)) <$> pValue <* string " goes to " <*> pBot

--------------------------------------------------------------------------------

main :: IO ()
main = do
  (bs,ins) <- partitionEithers . parseLines pInput <$> getContents
  let net = runNetwork (toRules bs) ins
  putStrLn $ "----[ Part  I ]----"
  mapM_ print [ botID | (botID, (_, hist)) <- IM.toList $ (net ^. bots), elem 61 hist, elem 17 hist ]
  putStrLn $ "----[ Part II ]----"
  print . product $ concatMap ((net ^. outputs) IM.!) [0..2]
