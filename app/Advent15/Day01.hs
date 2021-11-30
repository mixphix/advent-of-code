module Day01 where

in01 :: String
in01 = toString $ input 2015 1

dir :: Char -> Integer
dir = \case
  '(' -> 1
  ')' -> (-1)
  _ -> 0

part1 :: Integer
part1 = sumOn dir in01

part2 :: Int
part2 = elemIndex (-1) (scanl' (\acc x -> acc + dir x) 0 in01) ?: 0
