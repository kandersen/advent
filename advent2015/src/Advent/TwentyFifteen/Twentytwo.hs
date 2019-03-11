{-# LANGUAGE TemplateHaskell #-}
module Advent.TwentyFifteen.Twentytwo where

import Control.Applicative
import Control.Monad.State
import Lens.Micro.Platform
import Data.Heap (Heap)
import qualified Data.Heap as Heap

import Advent.Library

pProblem :: Parser (Int,Int)
pProblem = (,) <$ string "Hit Points: " <*> natural <* space <* string "Damage: " <*> natural

data EffectType = Shield 
                | Poison
                | Recharge
                deriving Eq


data Effect = Eff {
    _effectType :: EffectType,
    _effectDur :: Int
}
makeLenses ''Effect

data GameState = GS {
  _bossHP :: Int,
  _bossDMG :: Int,
  _playerHP :: Int,
  _playerMana :: Int,
  _playerArmor :: Int,
  _effects :: [Effect],
  _manaExpended :: Int
} 
makeLenses ''GameState

isDone :: GameState -> Bool
isDone = (<= 0) . _bossHP

newtype ByMana = ByMana { unByMana :: GameState }

instance Eq ByMana where
  (ByMana gs1) == (ByMana gs2) = _manaExpended gs1 == _manaExpended gs2

instance Ord ByMana where
  compare (ByMana gs1) (ByMana gs2) = compare (_manaExpended gs1) (_manaExpended gs2)

initState :: Int -> Int -> GameState
initState bHP bDMG = GS {
    _bossHP = bHP,
    _bossDMG = bDMG,
    _playerHP = 250,
    _playerMana = 500,
    _playerArmor = 0,
    _effects = [],
    _manaExpended = 0
}

spendMana :: MonadPlus f => Int -> StateT GameState f ()
spendMana n = do
  m <- use playerMana
  if m >= n
    then do
      playerMana -= n
      manaExpended += n
    else lift empty

addEffect :: MonadPlus f => EffectType -> Int -> StateT GameState f ()
addEffect eff rounds = do
  activeEffects <- use effects
  if eff `elem` (_effectType <$> activeEffects)
    then empty
    else effects %= (Eff eff rounds:)

tryMagicMissile :: MonadPlus f => StateT GameState f ()
tryMagicMissile = do
  spendMana 53
  bossHP -= 4

tryDrain :: MonadPlus f => StateT GameState f ()
tryDrain = do
  spendMana 73
  bossHP -= 2
  playerHP += 2

tryShield :: MonadPlus f => StateT GameState f ()
tryShield = do
  spendMana 113
  addEffect Shield 6

tryPoison :: MonadPlus f => StateT GameState f ()
tryPoison = do
  spendMana 173
  addEffect Poison 3

tryRecharge :: MonadPlus f => StateT GameState f ()
tryRecharge = do
  spendMana 229
  addEffect Recharge 5

castSpell :: MonadPlus f => StateT GameState f ()
castSpell = tryMagicMissile
        <|> tryDrain
        <|> tryShield
        <|> tryPoison
        <|> tryRecharge

tickEffects :: MonadPlus f => StateT GameState f ()
tickEffects = 

tickEffect :: MonadPlus f => EffectType -> StateT GameState f ()
tickEffect Shield = return ()
tickEffect Poison = bossDMG -= 3
tickEffect Recharge = playerMana += 101

step :: GameState -> [GameState]
step gs = flip execStateT gs $ do
  tickEffects
  castSpell 
  bossTurn

bossTurn :: MonadPlus f => StateT GameState f ()
bossTurn = do
  dmg <- use bossDMG
  arm <- use playerArmor
  hp <- playerHP <%= flip (-) ((dmg - arm) `max` 0)
  when (hp <= 0) empty
  
type Searcher a = State (Heap ByMana) a

search :: GameState -> [GameState]
search = evalState run . Heap.singleton . ByMana
  where
    run :: Searcher [GameState]
    run = do
      mnext <- Heap.viewMin <$> get
      case mnext of
        Nothing -> return []
        Just (ByMana x, h') ->
          if isDone x
            then return [x]
            else do
              put h'
              forM_ (step x) $ modify . Heap.insert . ByMana
              (x:) <$> run

main :: IO ()
main = defaultMain "2015.22" pProblem $ \(bossHP, bossDMG) -> do
  let initial = initState bossHP bossDMG
  let trace = search initial
  print $ fmap _manaExpended $ trace