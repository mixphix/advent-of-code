module Day25 where

uncantor :: Point 2 Natural -> Int
uncantor (Point2 r c) =
  let n = sq (r + c - 2) + r + (3 * c) - 4
   in succ . fromIntegral $ n `div` 2

in25 :: Point 2 Natural
in25 =
  let [a, b] = parse number <$> words (input 2015 25)
   in Point2 a b

codes :: [Natural]
codes = go 20151125
 where
  go n = n : go (n * 252533 `rem` 33554393)

part1 :: Natural
part1 = withNonEmpty 0 head $ drop (pred $ uncantor in25) codes
