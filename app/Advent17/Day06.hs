module Day06 where

import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as VM

in06 :: V.Vector Int
in06 = relist . mapMaybe (parsedWith number) $ words (input 2017 6)

stepFrom :: Int -> Int -> V.Vector Int -> V.Vector Int
stepFrom _ 0 v = v
stepFrom i q v
  | i == V.length v = stepFrom 0 q v
  | otherwise = stepFrom (succ i) (pred q) $ V.modify (\w -> VM.modify w succ i) v

step :: V.Vector Int -> V.Vector Int
step v =
  let m = V.maximum v
      Just i = V.elemIndex m v
   in stepFrom (succ i) m $ V.modify (\w -> VM.write w i 0) v

part1 :: Int
part1 = length $ iterateWhileUnique step in06

part2 :: Int
part2 = length . withNonEmpty [] (iterateWhileUnique step . last) . take part1 $ iterate step in06
