module Day03 where

three :: Parser (Point 3 Natural)
three = do
  x <- many space *> number
  y <- many space *> number
  z <- many space *> number
  pure $ Point3 x y z

triangle :: Point 3 Natural -> Bool
triangle (Point3 x y z) =
  let [a, b, c] = sort [x, y, z]
   in a + b > c

in03 :: [Point 3 Natural]
in03 = mapMaybe (parsedWith three) $ lines (input 2016 3)

part1 :: Natural
part1 = count triangle in03

part2 :: Natural
part2 =
  let in03chunks = chunksOf 3 in03
      f [Point3 a b c, Point3 i j k, Point3 x y z] = [Point3 a i x, Point3 b j y, Point3 c k z]
      f _ = []
   in count triangle $ foldMap f in03chunks
