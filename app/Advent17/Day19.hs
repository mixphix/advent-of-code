module Day19 where

import Advent
import Data.List ((\\))
import Data.Map.Strict qualified as Map

in19 :: Map (V2 Integer) Char
in19 = Map.mapKeys (fmap fromIntegral) . Map.filter (/= ' ') . view grid . map toString $ lines (input 2017 19)

start :: V2 Integer
start = find (elem 0) (keys in19) ?: V2 0 0

chug :: Map (V2 Integer) Char -> Ant -> Maybe Ant
chug m ant = case m !? p of
  Nothing -> Nothing
  Just '+' -> case filter (`member` m) (vonNeumann p) \\ [antPosition ant] of
    [v] -> case p - v of
      V2 0 1 -> Just $ Ant South p
      V2 1 0 -> Just $ Ant West p
      V2 0 (-1) -> Just $ Ant North p
      _ -> Just $ Ant East p
    _ -> Nothing
  Just _ -> Just step
  where
    step = scurry 1 ant
    p = antPosition step

trail :: [V2 Integer]
trail = unfoldr (fmapToFst antPosition . chug in19) (Ant North start)

part1 :: String
part1 = mapMaybe (guarded isAlpha <=< (in19 !?)) trail

part2 :: Int
part2 = succ {- doesn't count starting position -} $ length trail
