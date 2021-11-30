module Day12 where

import Data.Graph (Graph, buildG, reachable, scc)
import Data.Map.Strict qualified as Map

edgeP :: Parser (Int, [Int])
edgeP = do
  me <- number <* string " <-> "
  them <- number `sepBy1` string ", "
  pure (me, them)

in12 :: Graph
in12 =
  let m = relist $ parse edgeP <$> lines (input 2017 12)
      (lo, _) = Map.findMin m
      (hi, _) = Map.findMax m
   in buildG (lo, hi) $ Map.foldMapWithKey (\k v -> (k,) <$> v) m

part1 :: Int
part1 = length $ reachable in12 0

part2 :: Int
part2 = length $ scc in12
