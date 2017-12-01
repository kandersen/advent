module Main where

--import qualified Advent.TwentySixteen (getMain)
import qualified Advent.TwentySeventeen (getMain)
import System.Environment

main :: IO ()
main = do
  [year, day] <- getArgs
  case year of
--    "2016" -> Advent.TwentySixteen.getMain day
    "2017" -> Advent.TwentySeventeen.getMain day