module Advent.TwentySixteen ( 
  main,
  getMain
)where

import System.Environment (getArgs)

import qualified Advent.TwentySixteen.One (main)
import qualified Advent.TwentySixteen.Two (main)
import qualified Advent.TwentySixteen.Three (main)
import qualified Advent.TwentySixteen.Four (main)
import qualified Advent.TwentySixteen.Five (main)
import qualified Advent.TwentySixteen.Six (main)
import qualified Advent.TwentySixteen.Eight (main)
import qualified Advent.TwentySixteen.Nine (main)
import qualified Advent.TwentySixteen.Eleven (main)
import qualified Advent.TwentySixteen.Twelve (main)
import qualified Advent.TwentySixteen.Fifteen (main)
import qualified Advent.TwentySixteen.Sixteen (main)
import qualified Advent.TwentySixteen.Seventeen (main)
import qualified Advent.TwentySixteen.Eighteen (main)
import qualified Advent.TwentySixteen.Twentythree (main)

getMain :: String -> IO ()
getMain day = case day of
  "1" -> Advent.TwentySixteen.One.main
  "2" -> Advent.TwentySixteen.Two.main
  "3" -> Advent.TwentySixteen.Three.main
  "4" -> Advent.TwentySixteen.Four.main
  "5" -> Advent.TwentySixteen.Five.main
  "6" -> Advent.TwentySixteen.Six.main
  "8" -> Advent.TwentySixteen.Eight.main
  "9" -> Advent.TwentySixteen.Nine.main
  "11" -> Advent.TwentySixteen.Eleven.main
  "12" -> Advent.TwentySixteen.Twelve.main
  "15" -> Advent.TwentySixteen.Fifteen.main
  "16" -> Advent.TwentySixteen.Sixteen.main
  "17" -> Advent.TwentySixteen.Sixteen.main
  "18" -> Advent.TwentySixteen.Eighteen.main
  "23" -> Advent.TwentySixteen.Twentythree.main
  _ -> error $ "Unimplemented 2016 day: " ++ day

main :: IO ()
main = getMain . head =<< getArgs
