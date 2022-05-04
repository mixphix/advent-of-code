module Day21 where

type Grid = [String]

begin :: Grid
begin = [".#.", "..#", "###"]

gridP :: Parser Grid
gridP = many (oneOf ".#") `sepBy` string "/"

ruleP :: Parser [(Grid, Grid)]
ruleP = do
  source <- gridP
  target <- string " => " *> gridP
  pure $ (,target) . (`dihedralGrid` source) <$> universe

in21 :: Map Grid Grid
in21 = relist $ fold . parse ruleP <$> lines (input 2017 21)

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
part1 = count (== '#') . fold $ applyN 5 grow begin

part2 :: Natural
part2 = count (== '#') . fold $ applyN 18 grow begin
