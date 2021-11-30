module Day20 where

in20 :: Natural
in20 = parsedWith number (input 2015 20) ?: 0

part1 :: Int
part1 = maybe 0 succ $ findIndex ((in20 <=) . sumOn (10 *) . divisors) [1 ..]

part2 :: Int
part2 = maybe 0 succ $ findIndex (\k -> in20 <= sumOn (\d -> if 50 * d >= k then 11 * d else 0) (divisors k)) [1 ..]
