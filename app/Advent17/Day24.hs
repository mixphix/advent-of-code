module Day24 where

import Advent
import Data.List.Toolbox (groupSortOn, (\\))
import Data.Sequence qualified as Seq

data Component = Component Natural Natural
  deriving (Show)

instance Eq Component where
  Component x y == Component z w =
    (x == z && y == w) || (x == w && y == z)

canonical :: Component -> Component
canonical (Component x y) = Component (min x y) (max x y)

strength :: Component -> Natural
strength (Component x y) = x + y

pin :: Natural -> Component -> Maybe Component
pin p (Component x y)
  | p == x = Just (Component x y)
  | p == y = Just (Component y x)
  | otherwise = Nothing

bridgify :: [Component] -> Seq Component -> [Seq Component]
bridgify cs Seq.Empty =
  let zeros = mapMaybe (pin 0) cs
   in zeros >-< \z -> bridgify (map canonical cs \\ [canonical z]) (one z)
bridgify cs bs@(_ Seq.:|> Component _ p) =
  let nexts = mapMaybe (pin p) cs
   in case nexts of
        [] -> [bs]
        ns -> ns >-< \n -> bridgify (map canonical cs \\ [canonical n]) (bs Seq.:|> n)

in24 :: [Component]
in24 = mapMaybe (parsedWith $ Component <$> (number <* string "/") <*> number) $ lines (input 2017 24)

part1 :: Natural
part1 = maximumOf (sumOn strength) (bridgify in24 Empty) ?: 0

part2 :: Natural
part2 = (maximumOf (sumOn strength) <=< viaNonEmpty head) (groupSortOn (Down . length) (bridgify in24 Empty)) ?: 0
