module Advent.TwentySeventeen ( 
  main,
  getMain
)where

import System.Environment (getArgs)

import qualified Advent.TwentySeventeen.One (main)
import qualified Advent.TwentySeventeen.Two (main)
import qualified Advent.TwentySeventeen.Three (main)
import qualified Advent.TwentySeventeen.Four (main)
import qualified Advent.TwentySeventeen.Five (main)
import qualified Advent.TwentySeventeen.Six (main)
import qualified Advent.TwentySeventeen.Eight (main)
import qualified Advent.TwentySeventeen.Nine (main)
import qualified Advent.TwentySeventeen.Eleven (main)
import qualified Advent.TwentySeventeen.Twelve (main)
import qualified Advent.TwentySeventeen.Fifteen (main)
import qualified Advent.TwentySeventeen.Sixteen (main)
import qualified Advent.TwentySeventeen.Seventeen (main)
import qualified Advent.TwentySeventeen.Eighteen (main)
import qualified Advent.TwentySeventeen.TwentyThree (main)

getMain :: String -> IO ()
getMain day = case day of
  "1" -> Advent.TwentySeventeen.One.main
  "2" -> Advent.TwentySeventeen.Two.main
  "3" -> Advent.TwentySeventeen.Three.main
  "4" -> Advent.TwentySeventeen.Four.main
  "5" -> Advent.TwentySeventeen.Five.main
  "6" -> Advent.TwentySeventeen.Six.main
  "8" -> Advent.TwentySeventeen.Eight.main
  "9" -> Advent.TwentySeventeen.Nine.main
  "11" -> Advent.TwentySeventeen.Eleven.main
  "12" -> Advent.TwentySeventeen.Twelve.main
  "15" -> Advent.TwentySeventeen.Fifteen.main
  "16" -> Advent.TwentySeventeen.Sixteen.main
  "17" -> Advent.TwentySeventeen.Seventeen.main
  "18" -> Advent.TwentySeventeen.Eighteen.main
  "23" -> Advent.TwentySeventeen.TwentyThree.main
  _ -> error $ "Unimplemented 2017 day: " ++ day

main :: IO ()
main = getMain . head =<< getArgs
