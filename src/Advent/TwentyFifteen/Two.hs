module Two where
import Advent

type BoxDims = (Int, Int, Int)

paperForPresent :: BoxDims -> Int
paperForPresent (w, h, l) = sum ((2*) <$> surfaces) + minimum surfaces
  where
    surfaces = [l * w, w * h, h * l]

ribbonForPresent :: BoxDims -> Int
ribbonForPresent (w, h, l) = minimum perimeters + (w * h * l)
  where
    perimeters = [2 * w + 2 * h, 2 * h + 2 * l, 2 * w + 2 * l]

parseLine :: String -> BoxDims
parseLine input =
  let [w, h, l] = read <$> splitOn 'x' input
  in (w, h, l)

main :: IO ()
main = defaultMain $ \input -> do
  let dimensions = parseLine <$> lines input
  print $ sum . map paperForPresent $ dimensions
  print $ sum . map ribbonForPresent $ dimensions
