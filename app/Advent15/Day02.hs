module Day02 where

import Advent

cube :: Parser (Point 3 Natural)
cube = do
  n1 <- number <* char 'x'
  n2 <- number <* char 'x'
  Point3 n1 n2 <$> number

in02 :: [Point 3 Natural]
in02 = mapMaybe (parsedWith cube) $ lines (input 2015 2)

part1 :: Natural
part1 = sumOn (((+) <$> sumOn (2 *) <*> minimum) . areas) in02
  where
    areas (Point3 l w h) = Point3 (l * w) (w * h) (h * l)

part2 :: Natural
part2 = sumOn ((+) <$> minimum . perimeters <*> volume) in02
  where
    perimeters (Point3 l w h) = Point3 (2 * l + 2 * w) (2 * w + 2 * h) (2 * h + 2 * l)
    volume (Point3 l w h) = l * w * h
