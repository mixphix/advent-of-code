module Day17 where

import Advent
import Data.Sequence qualified as Seq

spinlock :: Part -> Int -> Int -> Int -> Seq Int -> Seq Int
spinlock _ 0 _ _ v = v
spinlock part n step pos0 v =
  let pos = succ $ (pos0 + step) `mod` length v
      inserted = (case part of Part1 -> 2018; _ -> 50000001) - n
   in spinlock part (pred n) step pos $ Seq.insertAt pos inserted v

in17 :: Int
in17 = parsedWith number (input 2017 17) ?: 0

part1 :: Int
part1 =
  let spun = spinlock Part1 2017 in17 0 (one 0)
      Just k = Seq.elemIndexL 2017 spun
   in Seq.index spun (succ k `mod` length spun)

part2 :: Int
part2 =
  let spun = spinlock Part2 50000000 in17 0 (one 0)
      Just k = Seq.elemIndexL 0 spun
   in Seq.index spun (succ k `mod` length spun)
