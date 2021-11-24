module Day15 where

import Advent
import Data.Bits ((.&.))
import Data.List (iterate')
import Data.Text qualified as T

in15 :: (Word, Word)
in15 =
  let [a, b] = mapMaybe (parsedWith number . T.filter isDigit) $ lines (input 2017 15)
   in (a, b)

stepA, stepB :: Word -> Word
stepA a = (a * 16807) `rem` 2147483647
stepB b = (b * 48271) `rem` 2147483647

step :: Part -> (Word, Word) -> (Word, Word)
step Part1 (a, b) = (stepA a, stepB b)
step Part2 (a, b) = (find (multipleOf 4) (drop 1 $ iterate' stepA a) ?: 0, find (multipleOf 8) (drop 1 $ iterate' stepB b) ?: 0)

low16Match :: Word -> Word -> Bool
low16Match a b = (a .&. 0xffff) == (b .&. 0xffff)

part1 :: Natural
part1 = count (uncurry low16Match) . take (4 * 10 ^ (7 :: Int)) . drop 1 $ iterate' (step Part1) in15

part2 :: Natural
part2 = count (uncurry low16Match) . take (5 * 10 ^ (6 :: Int)) . drop 1 $ iterate' (step Part2) in15
