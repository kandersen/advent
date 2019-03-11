module Advent.TwentyFifteen ( 
  main,
  getMain
)where

import System.Environment (getArgs)

import qualified Advent.TwentyFifteen.One (main)
import qualified Advent.TwentyFifteen.Two (main)
import qualified Advent.TwentyFifteen.Three (main)
import qualified Advent.TwentyFifteen.Four (main)
import qualified Advent.TwentyFifteen.Five (main)
import qualified Advent.TwentyFifteen.Six (main)
import qualified Advent.TwentyFifteen.SixB (main)
import qualified Advent.TwentyFifteen.Seven (main)
import qualified Advent.TwentyFifteen.Eight (main)
import qualified Advent.TwentyFifteen.Nine (main)
import qualified Advent.TwentyFifteen.Ten (main)
import qualified Advent.TwentyFifteen.Eleven (main)
import qualified Advent.TwentyFifteen.Twelve (main)
import qualified Advent.TwentyFifteen.Thirteen (main)
import qualified Advent.TwentyFifteen.Fourteen (main)
import qualified Advent.TwentyFifteen.Fifteen (main)
import qualified Advent.TwentyFifteen.Twentytwo (main)
import qualified Advent.TwentyFifteen.Twentythree (main)

getMain :: String -> IO ()
getMain day = case day of
  "1" -> Advent.TwentyFifteen.One.main
  "2" -> Advent.TwentyFifteen.Two.main
  "3" -> Advent.TwentyFifteen.Three.main
  "4" -> Advent.TwentyFifteen.Four.main
  "5" -> Advent.TwentyFifteen.Five.main
  "6" -> Advent.TwentyFifteen.Six.main
  "6b" -> Advent.TwentyFifteen.SixB.main
  "7" -> Advent.TwentyFifteen.Seven.main
  "8" -> Advent.TwentyFifteen.Eight.main
  "9" -> Advent.TwentyFifteen.Nine.main
  "10" -> Advent.TwentyFifteen.Ten.main
  "11" -> Advent.TwentyFifteen.Eleven.main
  "12" -> Advent.TwentyFifteen.Twelve.main
  "13" -> Advent.TwentyFifteen.Thirteen.main
  "14" -> Advent.TwentyFifteen.Fourteen.main
  "15" -> Advent.TwentyFifteen.Fifteen.main
  "22" -> Advent.TwentyFifteen.Twentytwo.main
  "23" -> Advent.TwentyFifteen.Twentythree.main
  _ -> error $ "Unimplemented 2015 day: " ++ day

main :: IO ()
main = getMain . head =<< getArgs
