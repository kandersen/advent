module Advent.TwentyEighteen where

import qualified Advent.TwentyEighteen.One
import qualified Advent.TwentyEighteen.Two
import qualified Advent.TwentyEighteen.Three
import qualified Advent.TwentyEighteen.Four
import qualified Advent.TwentyEighteen.Five
import qualified Advent.TwentyEighteen.Six
import qualified Advent.TwentyEighteen.Seven
import qualified Advent.TwentyEighteen.SevenB
import qualified Advent.TwentyEighteen.Eight
import qualified Advent.TwentyEighteen.Nine
import qualified Advent.TwentyEighteen.NineB
import qualified Advent.TwentyEighteen.Ten
import qualified Advent.TwentyEighteen.Twelve
import qualified Advent.TwentyEighteen.Fourteen
import qualified Advent.TwentyEighteen.Sixteen
import qualified Advent.TwentyEighteen.Eighteen
import qualified Advent.TwentyEighteen.Nineteen

import Data.Map as Map
import System.Environment

appMap :: Map String (IO ())
appMap = Map.fromList [
    ("1", Advent.TwentyEighteen.One.main) 
  , ("2", Advent.TwentyEighteen.Two.main)
  , ("3", Advent.TwentyEighteen.Three.main)
  , ("4", Advent.TwentyEighteen.Four.main)
  , ("5", Advent.TwentyEighteen.Five.main)
  , ("6", Advent.TwentyEighteen.Six.main)
  , ("7", Advent.TwentyEighteen.Seven.main)
  , ("7b", Advent.TwentyEighteen.SevenB.main)
  , ("8", Advent.TwentyEighteen.Eight.main)
  , ("9", Advent.TwentyEighteen.Nine.main)
  , ("9b", Advent.TwentyEighteen.NineB.main)
  , ("10", Advent.TwentyEighteen.Ten.main)
  , ("12", Advent.TwentyEighteen.Twelve.main)
  , ("14", Advent.TwentyEighteen.Fourteen.main)
  , ("16", Advent.TwentyEighteen.Sixteen.main)
  , ("18", Advent.TwentyEighteen.Eighteen.main)
  , ("19", Advent.TwentyEighteen.Nineteen.main)
  ]

main :: IO ()
main = (appMap !) . head =<< getArgs