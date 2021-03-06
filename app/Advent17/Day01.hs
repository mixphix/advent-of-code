module Day01 where

import Data.Text qualified as T

in01 :: [Natural]
in01 = parse number . T.singleton <$> toString (input 2017 1)

part1 :: Natural
part1 = sumOn head . filter (allSame . relist) $ pairsTied in01

part2 :: Natural
part2 =
  let l = zipWith list2 in01 $ drop (length in01 `div` 2) (cycle in01)
   in sumOn (withNonEmpty 0 head) $ filter (allSame . relist) l
