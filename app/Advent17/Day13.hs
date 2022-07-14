module Day13 where

in13 :: [(Int, Int)]
in13 =
  parse ((,) <$> number <*> (string ": " *> number)) <$> lines (input 2017 13)

part1 :: Int
part1 =
  sumOn (\(d, r) -> if d `mod` (2 * r - 2) == 0 then d * r else 0) in13

part2 :: Int
part2 =
  find (\n -> all (\(d, r) -> (d + n) `mod` (2 * r - 2) /= 0) in13) [1 ..] ?: 0
