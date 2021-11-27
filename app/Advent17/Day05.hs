module Day05 where

import Advent hiding (Vector)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as VM

in05 :: Vector Int
in05 = relist . mapMaybe (parsedWith number) $ lines (input 2017 5)

data Program = Program {instructions :: Vector Int, position :: Int} deriving (Eq)

instruction :: Part -> Program -> Natural
instruction pt (Program v n)
  | n < 0 || n >= V.length v = 0
  | pt == Part1 = succ . instruction pt $ Program (V.modify (\w -> VM.modify w succ n) v) (n + v V.! n)
  | otherwise = case v V.! n of
    vn
      | vn >= 3 -> succ . instruction pt $ Program (V.modify (\w -> VM.modify w pred n) v) (n + vn)
      | otherwise -> succ . instruction pt $ Program (V.modify (\w -> VM.modify w succ n) v) (n + vn)

part1 :: Natural
part1 = instruction Part1 $ Program in05 0

part2 :: Natural
part2 = instruction Part2 $ Program in05 0
