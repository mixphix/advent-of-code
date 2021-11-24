module Day14 where

import Advent
import Data.Bits (testBit)
import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Text qualified as T
import Day10 (knotHash)

in14 :: [Text]
in14 =
  let i = T.strip $ input 2017 14
   in [0 .. 127 :: Word8] <&> \n -> i <> "-" <> show n

used :: Set (V2 Integer)
used =
  Set.map (fmap fromIntegral) . Map.keysSet . Map.mapMaybe (? ()) . view grid $
    foldMap (flip testBit <$> [7, 6 .. 0] ??) . knotHash <$> in14

part1 :: Int
part1 = length used

contiguous :: Set (V2 Integer) -> Set (V2 Integer) -> Set (V2 Integer)
contiguous vs set =
  case Set.intersection set $ relist (foldMap vonNeumann vs) \\ vs of
    Empty -> vs
    ws -> contiguous (vs <> ws) (set \\ (vs <> ws))

regions :: Set (V2 Integer) -> Integer
regions = go 0
  where
    go :: Integer -> Set (V2 Integer) -> Integer
    go n Empty = n
    go n set =
      let Just v = minimumOn sum set
       in go (succ n) $! set \\ contiguous (one v) set

part2 :: Integer
part2 = regions used
