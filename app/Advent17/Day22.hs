module Day22 where

import Data.Map.Strict qualified as Map

virus :: Part -> (Ant, Map (Point 2 Integer) Char) -> (Ant, Map (Point 2 Integer) Char)
virus Part1 (a@(view position -> p), m) =
  case m !? p of
    Just '#' -> (scurry 1 $ turnR a, m & at p ?~ '.')
    _ -> (scurry 1 $ turnL a, m & at p ?~ '#')
virus Part2 (a@(view position -> p), m) =
  case m !? p of
    Just '#' -> (scurry 1 $ turnR a, m & at p ?~ 'F')
    Just 'F' -> (scurry 1 $ turnU a, m & at p ?~ '.')
    Just 'W' -> (scurry 1 a, m & at p ?~ '#')
    _ -> (scurry 1 $ turnL a, m & at p ?~ 'W')

in22 :: Map (Point 2 Integer) Char
in22 = Map.mapKeys (dihedralTransform TR2 . fmap fromIntegral) . view grid . map toString $ lines (input 2017 22)

start :: Ant
start = Ant North . fmap (`div` 2) . withNonEmpty origin (maximumOn1 (manhattan origin)) $ keys in22

part1 :: Natural
part1 =
  count (\(a, m) -> m !? antPosition a /= Just '#')
    . take 10000
    $ iterate (virus Part1) (start, in22)

part2 :: Natural
part2 =
  count (\(a, m) -> m !? antPosition a == Just 'W')
    . take 10000000
    $ iterate (virus Part2) (start, in22)
