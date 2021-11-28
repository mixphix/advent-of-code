module Day21 where

import Advent
import Data.List.Toolbox (chunksOf)

type Grid = [String]

begin :: Grid
begin = [".#.", "..#", "###"]

gridP :: Parser Grid
gridP = many (oneOf ".#") `sepBy` string "/"

ruleP :: Parser [(Grid, Grid)]
ruleP = do
  source <- gridP
  target <- string " => " *> gridP
  let sources = universe <&> (`dihedralGrid` source)
  pure $ map (,target) sources

in21 :: Map Grid Grid
in21 = relist . fold . mapMaybe (parsedWith ruleP) $ lines (input 2017 21)

gridChunks :: Int -> Grid -> [[Grid]]
gridChunks k = transpose . map (chunksOf k) . transpose . map (chunksOf k)

glueChunks :: [[Grid]] -> Grid
glueChunks = map fold . foldMap transpose

grow :: Grid -> Grid
grow g
  | multipleOf 2 (length g) =
    let chunks = gridChunks 2 g
     in glueChunks $ chunks <&> \chunkRow -> chunkRow <&> \chunk -> in21 !? chunk ?: []
  | otherwise =
    let chunks = gridChunks 3 g
     in glueChunks $ chunks <&> \chunkRow -> chunkRow <&> \chunk -> in21 !? chunk ?: []

part1 :: Natural
part1 = count (== '#') . fold $ applyN (5 :: Int) grow begin

part2 :: Natural
part2 = count (== '#') . fold $ applyN (18 :: Int) grow begin