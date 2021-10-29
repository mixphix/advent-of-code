module Day09 where

import Advent
import Data.List.NonEmpty.Toolbox (groupWith, maximumOn1)
import Data.Map.Monoidal.Strict ((!))
import Data.Map.Monoidal.Strict qualified as Mop
import Data.Set qualified as Set

type Routes = Mop.Map String (Mop.Map String (Sum Natural))

route :: Parser [(String, (String, Sum Natural))]
route = do
  source <- many1 alphaNum <* string " to "
  target <- many1 alphaNum <* string " = "
  dist <- number
  pure [(source, (target, dist)), (target, (source, dist))]

in09 :: Routes
in09 =
  relist
    . map (fst . head &&& foldMap (one . snd))
    . groupWith fst
    . sort
    . foldMap (fromMaybe [] . parsedWith route)
    $ lines (input 2015 9)

places :: Set String
places = Mop.keysSet in09

shortestRoute :: String -> Natural
shortestRoute k0 = getSum $ go (Set.delete k0 places) (Sum 0) k0
  where
    go remaining n k
      | null remaining = n
      | otherwise =
        let visited = Mop.restrictKeys (in09 ! k) remaining
            Just (p, pn) = minimumOn snd $ Mop.assocs visited
         in go (Set.delete p remaining) (n <> pn) p

longestRoute :: String -> Natural
longestRoute k0 = getSum $ go (Set.delete k0 places) (Sum 0) k0
  where
    go remaining n k
      | null remaining = n
      | otherwise =
        let visited = Mop.restrictKeys (in09 ! k) remaining
            (p, pn) = maximumOn1 snd . relist $ Mop.assocs visited
         in go (Set.delete p remaining) (n <> pn) p

part1 :: Natural
part1 = minimumOf shortestRoute places ?: 0

part2 :: Natural
part2 = maximumOf longestRoute places ?: 0
