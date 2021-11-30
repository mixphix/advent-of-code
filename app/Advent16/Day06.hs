module Day06 where

import Data.Text qualified as T

in06 :: [String]
in06 = map toString . T.transpose $ lines (input 2016 6)

part1 :: String
part1 =
  mapMaybe (viaNonEmpty head <=< viaNonEmpty head) $
    sortOn (Down . length) . groupSort <$> in06

part2 :: String
part2 =
  mapMaybe (viaNonEmpty head <=< viaNonEmpty head) $
    sortOn length . groupSort <$> in06
