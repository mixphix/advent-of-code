module Day05 where

import Data.Text qualified as T

in05 :: [Text]
in05 = filter (/= "") $ lines (input 2015 5)

nice1 :: Text -> Bool
nice1 t@(toString -> tstr) =
  none (`T.isInfixOf` t) ["ab", "cd", "pq", "xy"]
    && hasMoreThan 3 vowel tstr
    && or (pairwise (==) tstr)
 where
  vowel c = c `elem` ("aeiou" :: String)

part1 :: Natural
part1 = count nice1 in05

nice2 :: Text -> Bool
nice2 (toString -> tstr) =
  hasMatchingPair && hasABApattern
 where
  hasABApattern = uncurry (on (||) (or . pairwise (==))) $ uninterleave tstr
  hasMatchingPair =
    anySame
      . foldMap (\xs -> if length xs > 2 then take 2 xs else take 1 xs)
      $ group (pairsTied tstr)

part2 :: Natural
part2 = count nice2 in05
