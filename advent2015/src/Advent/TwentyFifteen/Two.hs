module Advent.TwentyFifteen.Two where
import Advent.Library

type BoxDims = (Int, Int, Int)

pDims :: Parser BoxDims
pDims = (\x y z -> (x, y, z)) <$> natural <* char 'x' <*> natural <* char 'x' <*> natural

paperForPresent :: BoxDims -> Int
paperForPresent (w, h, l) = sum ((2*) <$> surfaces) + minimum surfaces
  where
    surfaces = [l * w, w * h, h * l]

ribbonForPresent :: BoxDims -> Int
ribbonForPresent (w, h, l) = minimum perimeters + (w * h * l)
  where
    perimeters = [2 * w + 2 * h, 2 * h + 2 * l, 2 * w + 2 * l]

main :: IO ()
main = defaultMain "2015.2" (pLines pDims) $ \dimensions -> do
  print $ sum . map paperForPresent $ dimensions
  print $ sum . map ribbonForPresent $ dimensions
