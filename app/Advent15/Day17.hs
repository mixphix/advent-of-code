module Day17 where

import Advent

in17 :: [Natural]
in17 = mapMaybe (parsedWith number) $ lines (input 2015 17)

part1 :: Natural
part1 = count ((150 ==) . sum) $ subsequences in17

part2 :: Natural
part2 =
  let combs = filter ((150 ==) . sum) $ subsequences in17
      Just shortest = minimumOn length combs
   in count (on (==) length shortest) combs
