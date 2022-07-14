module Day01 where

in01 :: [Integer]
in01 = parse number <$> lines (input 2021 1)

part1 :: Natural
part1 = withNonEmpty 0 (count (\(a :| [b]) -> a < b) . init) $ pairsTied in01

part2 :: Natural
part2 =
  let threes =
        zipWith3
          (\a b c -> a + b + c)
          in01
          (drop 1 in01)
          (drop 2 in01)
   in withNonEmpty 0 (count (\(a :| [b]) -> a < b) . init) $ pairsTied threes
