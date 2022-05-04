module Day09 where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

in09 :: Map (Point 2 Integer) Int
in09 = view grid $ map (parse number . one) . toString <$> lines (input 2021 9)

part1 :: Int
part1 =
  getSum $ Map.foldMapWithKey (\k v -> Sum $ if all (> v) (mapMaybe (in09 !?) (vonNeumann k)) then 1 + v else 0) in09

contiguous :: Set (Point 2 Integer) -> Set (Point 2 Integer) -> Set (Point 2 Integer)
contiguous vs set =
  case Set.intersection set $ relist (foldMap vonNeumann vs) Set.\\ vs of
    Empty -> vs
    ws -> contiguous (vs <> ws) (set Set.\\ (vs <> ws))

regionSizeSum :: Set (Point 2 Integer) -> [Integer]
regionSizeSum = go []
 where
  go :: [Integer] -> Set (Point 2 Integer) -> [Integer]
  go n Empty = n
  go n set =
    let v = withNonEmpty origin (minimumOn1 sum) set
        ctg = contiguous (one v) set
     in go (fromIntegral (Set.size ctg) : n) $! set Set.\\ ctg

part2 :: Integer
part2 = product . take 3 . sortOn Down . regionSizeSum . Map.keysSet $ Map.filter (/= 9) in09
