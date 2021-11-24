module Day02 where

import Advent
import Data.List ((\\))

in02 :: [NonEmpty Natural]
in02 = mapMaybe (nonEmpty <=< parsedWith (number `sepBy` char '\t')) $ lines (input 2017 2)

part1 :: Natural
part1 = sumOn (liftM2 (-) maximum minimum) in02

part2 :: Natural
part2 =
  let divved :: NonEmpty Natural -> [Natural]
      divved (toList -> ns) = [q | m <- ns, n <- ns \\ [m], let (q, r) = divMod m n, r == 0]
   in sumOn (sum . divved) in02
