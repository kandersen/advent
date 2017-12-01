module Advent.TwentySeventeen ( 
  getMain
)where

import qualified Advent.TwentySeventeen.One (main)

getMain :: String -> IO ()
getMain day = case day of
  "1" -> Advent.TwentySeventeen.One.main
  _ -> error $ "Unimplemented 2017 day: " ++ day
