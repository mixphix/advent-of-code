module Day07 where

import Data.Text qualified as T

in07 :: NonEmpty Natural
in07 = relist . sort $ parse number <$> T.splitOn "," (input 2021 7)

lo :: Natural
hi :: Natural
(lo, hi) = coerce $ foldMap1 (Min &&& Max) in07

sep :: Natural -> Natural -> Natural
sep n k
  | k > n = k - n
  | otherwise = n - k

part1 :: Natural
part1 = withNonEmpty 0 (minimumOf1 $ \n -> sumOn (sep n) in07) [lo .. hi]

part2 :: Natural
part2 = withNonEmpty 0 (minimumOf1 $ \n -> sumOn (ngon 3 . sep n) in07) [lo .. hi]
