module Day03 where

import Advent
import Data.List.Toolbox (chunksOf)

three :: Parser (V3 Natural)
three = do
  x <- many space *> number
  y <- many space *> number
  z <- many space *> number
  pure $ V3 x y z

triangle :: V3 Natural -> Bool
triangle (V3 x y z) =
  let [a, b, c] = sort [x, y, z]
   in a + b > c

in03 :: [V3 Natural]
in03 = mapMaybe (parsedWith three) $ lines (input 2016 3)

part1 :: Natural
part1 = count triangle in03

part2 :: Natural
part2 =
  let in03chunks = chunksOf 3 in03
      f [V3 a b c, V3 i j k, V3 x y z] = [V3 a i x, V3 b j y, V3 c k z]
      f _ = []
   in count triangle $ foldMap f in03chunks
