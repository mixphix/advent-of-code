module Day05 where

import Data.Text qualified as T

in05 :: Text
in05 = input 2018 5

units :: [Text]
units = join (zipWith (on (<>) T.singleton)) (['A' .. 'Z'] <> ['a' .. 'z'])

react :: Text -> Text
react = withNonEmpty "" last . iterateWhileUnique (go units)
 where
  go (u : us) t = go us . fold $ T.splitOn u t
  go [] t = t

part1 :: Int
part1 = T.length $ react in05

part2 :: Int
part2 =
  withNonEmpty 0 (minimumOf1 (T.length . react)) $
    ['a' .. 'z'] <&> \c -> fold $ T.split (`elem` [c, toUpper c]) in05
