module Day14 where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Day10 (knotHash)

in14 :: [Text]
in14 =
  let i = input 2017 14
   in [0 .. 127 :: Word8] <&> \n -> i <> "-" <> show n

used :: Set (Point 2 Integer)
used =
  Set.map (fmap fromIntegral) . Map.keysSet . Map.mapMaybe (? ()) . view grid $
    foldMap (flip testBit <$> [7, 6 .. 0] ??) . knotHash <$> in14

part1 :: Int
part1 = length used

contiguous ::
  Set (Point 2 Integer) ->
  Set (Point 2 Integer) ->
  Set (Point 2 Integer)
contiguous vs set =
  case Set.intersection set $ relist (foldMap vonNeumann vs) Set.\\ vs of
    Empty -> vs
    ws -> contiguous (vs <> ws) (set Set.\\ (vs <> ws))

regions :: Set (Point 2 Integer) -> Integer
regions = go 0
 where
  go :: Integer -> Set (Point 2 Integer) -> Integer
  go n Empty = n
  go n set =
    let v = withNonEmpty origin (minimumOn1 sum) set
     in go (succ n) $! set Set.\\ contiguous (one v) set

part2 :: Integer
part2 = regions used
