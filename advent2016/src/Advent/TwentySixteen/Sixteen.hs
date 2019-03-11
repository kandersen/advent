module Advent.TwentySixteen.Sixteen(main) where

stream :: String -> String
stream seed = result
  where
    result = init 0 seed

    init n (c:cs) = c : init (n + 1) cs
    init n []     = go n result

    go :: Int -> String -> String
    go chars = go' chars []
      where
        go' 0 acc _        = '0':acc ++ go (chars * 2 + 1) result
        go' n acc ('0':cs) = go' (n - 1) ('1':acc) cs
        go' n acc ('1':cs) = go' (n - 1) ('0':acc) cs

checksum :: String -> String
checksum s = go checksum s
  where
    go :: (String -> String) -> String -> String
    go k       []             = k []
    go _      [_]             = s
    go k (a:b:s') | a == b    = go (k . ('1':)) s'
                  | otherwise = go (k . ('0':)) s'
main :: IO ()
main = do
  seed <- getLine
  putStrLn "----[ Part  I ]----"
  putStrLn . checksum . take 272 . stream $ seed
  putStrLn "----[ Part II ]----"
  putStrLn . checksum . take 35651584 . stream $ seed
