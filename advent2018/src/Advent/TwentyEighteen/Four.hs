{-# LANGUAGE RecordWildCards #-}
module Advent.TwentyEighteen.Four where

import Advent.Library
import Data.Ord

import Control.Monad

import Data.List 
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Arrow

data Time = Time {
    year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int
} deriving (Eq)

instance Show Time where
    show Time{..} = 
      concat [show year, "-", show month, "-", show day, " ", show hour, ":", show minute]

instance Ord Time where
  compare t1 t2 = 
    comparing year t1 t2 <> 
    comparing month t1 t2 <> 
    comparing day t1 t2 <> 
    comparing hour t1 t2 <>
    comparing minute t1 t2

pTime :: Parser Time
pTime = Time <$> 
    natural <* char '-' <*> natural <* char '-' <*> natural <* space 
    <*> natural <* char ':' <*> natural

minutesTo :: Time -> Time -> [Int]
minutesTo t1 t2 = [minute t1 .. minute t2 - 1] 

data Event = WakesUp
           | FallsAsleep
           | BeginsShift Int
           deriving (Show, Eq)

pEvent :: Parser Event
pEvent = WakesUp <$ string "wakes up"
     <|> FallsAsleep <$ string "falls asleep"
     <|> BeginsShift <$ string "Guard #" <*> natural <* string " begins shift"

data Entry = Entry {
    time :: Time,
    event :: Event
} deriving (Eq)

instance Show Entry where
    show Entry{..} = 
        concat ["[", show time, "] ", show event]

instance Ord Entry where
    compare = comparing time

pEntry :: Parser Entry
pEntry = Entry <$
  char '[' <*> pTime <* char ']' <* space <*> pEvent

tallySleepingMinutes :: [Entry] -> Map Int [Int]
tallySleepingMinutes entries = execState (init entries) Map.empty
  where
    init (Entry{..} : es) = case event of
      BeginsShift guardID -> 
        awake guardID es
      _ -> error "No guard on initial duty"

    awake guard [] = return ()
    awake guard (Entry{..} : es) = case event of
      BeginsShift guardID -> awake guardID es
      FallsAsleep -> asleep guard time es
      WakesUp -> error "cannot wake up without falling asleep"
    
    asleep :: Int -> Time -> [Entry] -> State (Map Int [Int]) ()
    asleep guard timeAsleep [] = error $ "Guard #" ++ show guard ++ "never wakes up after " ++ show timeAsleep
    asleep guard timeAsleep (Entry{..} : es) = case event of
      BeginsShift _ -> error "Shift beginning before previous wakes up"
      FallsAsleep -> error "cannot sleep while sleeping"
      WakesUp -> do
        modify (Map.alter (insertOrAppend $ timeAsleep `minutesTo` time) guard)
        awake guard es    
    
    insertOrAppend ms Nothing = Just ms
    insertOrAppend ms (Just ms') = Just $ ms' ++ ms

mostFrequents :: [Int] -> [Int]
mostFrequents = maximumBy (comparing length) . group . sort

main :: IO ()
main = defaultMain "2018.4" (pLines pEntry) $ \entries -> do
    let tally = Map.assocs $ tallySleepingMinutes (sort entries)
    -- part 1
    let (guard, minute:_) = second mostFrequents . maximumBy (comparing (length . snd)) $ tally
    print $ guard * minute
    -- part 2
    let (guard, minute:_) = maximumBy (comparing (length . snd)) (second mostFrequents <$> tally)
    print $ guard * minute

