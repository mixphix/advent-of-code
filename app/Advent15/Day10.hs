module Day10 where

import Advent
import Data.List.NonEmpty (group1)

in10 :: [Natural]
in10 = relist . digits $ parsedWith number (input 2015 10) ?: 0

lookAndSay :: [Natural] -> [Natural]
lookAndSay = withNonEmpty [] (say . group1)
  where
    say (gs@(g :| _) :| gss) = count every gs : g : withNonEmpty [] say gss

part1 :: Int
part1 = maybe 0 length $ iterate lookAndSay in10 !!? 40

part2 :: Int
part2 = maybe 0 length $ iterate lookAndSay in10 !!? 50
