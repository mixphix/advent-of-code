module Day01 where

in01 :: [Integer]
in01 = parse (optional (char '+') *> number) <$> lines (input 2018 1)

part1 :: Integer
part1 = sum in01

part2 :: Integer
part2 = withNonEmpty 0 head . dropWhileUnique $ scanl' (+) 0 (cycle in01)
