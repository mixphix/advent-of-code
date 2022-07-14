module Day09 where

import Data.Map.Monoidal.Strict qualified as Mop
import Data.Set qualified as Set
import Data.Text qualified as T

in09 :: Mop (Point 2 Integer) (Sum Int)
in09 =
  relist . view grid $
    map (parse number . T.singleton) . toString <$> lines (input 2021 9)

part1 :: Int
part1 =
  getSum $
    Mop.foldMapWithKey
      (\k v -> all (> v) (mapMaybe (in09 !?) (vonNeumann k)) ?> 1 + v)
      in09

contiguous ::
  Set (Point 2 Integer) ->
  Set (Point 2 Integer) ->
  Set (Point 2 Integer)
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
part2 =
  product . take 3
    . sortOn Down
    . regionSizeSum
    . Mop.keysSet
    $ Mop.filter (/= 9) in09
