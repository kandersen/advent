module Advent.TwentyFifteen (getMain) where

--import qualified Advent.TwentyFifteen.Seven (main)

getMain :: String -> IO ()
getMain day = case day of
  --"7" -> Advent.TwentyFifteen.Seven.main
--"21" -> Advent.TwentySixteen.Twentyone.main
  _ -> return ()