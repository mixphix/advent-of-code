module Day02 where

import Advent

directions :: Parser Cardinal
directions = choice [North <$ char 'U', East <$ char 'R', West <$ char 'L', South <$ char 'D']

in02 :: [[Cardinal]]
in02 = mapMaybe (parsedWith $ many1 directions) $ lines (input 2016 2)

gridFor :: Part -> [V2 Integer]
gridFor Part1 = V2 0 0 : moore (V2 0 0)
gridFor Part2 = V2 0 0 : V2 2 0 : V2 (-2) 0 : V2 0 2 : V2 0 (-2) : moore (V2 0 0)

gridValue :: Part -> V2 Integer -> Char
gridValue Part1 = \case
  V2 (-1) 1 -> '1'
  V2 0 1 -> '2'
  V2 1 1 -> '3'
  V2 (-1) 0 -> '4'
  V2 0 0 -> '5'
  V2 1 0 -> '6'
  V2 (-1) (-1) -> '7'
  V2 0 (-1) -> '8'
  V2 1 (-1) -> '9'
  _ -> error "not in grid!"
gridValue Part2 = \case
  V2 0 2 -> '1'
  V2 (-1) 1 -> '2'
  V2 0 1 -> '3'
  V2 1 1 -> '4'
  V2 (-2) 0 -> '5'
  V2 (-1) 0 -> '6'
  V2 0 0 -> '7'
  V2 1 0 -> '8'
  V2 2 0 -> '9'
  V2 (-1) (-1) -> 'A'
  V2 0 (-1) -> 'B'
  V2 1 (-1) -> 'C'
  V2 0 (-2) -> 'D'
  _ -> error "not in grid!"

walkGrid :: Part -> Cardinal -> Ant -> Ant
walkGrid part c a@(antPosV -> v)
  | cardinal 1 c v `elem` gridFor part = shimmy 1 c a
  | otherwise = a

code :: Part -> V2 Integer -> [Cardinal] -> Char
code part pos =
  gridValue part . antPosV . foldl' (flip (walkGrid part)) (Ant (North, pos ^. complex))

part1 :: String
part1 = map (code Part1 (V2 0 0)) in02

part2 :: String
part2 = map (code Part2 (V2 (-2) 0)) in02
