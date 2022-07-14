module Day02 where

directions :: Parser Cardinal
directions =
  choice
    [ North <$ char 'U'
    , East <$ char 'R'
    , West <$ char 'L'
    , South <$ char 'D'
    ]

in02 :: [[Cardinal]]
in02 = parse (many1 directions) <$> lines (input 2016 2)

gridFor :: Part -> [Point 2 Integer]
gridFor Part1 = relist $ surrounding 1 (Point2 0 0)
gridFor Part2 = relist $ taxicab 2 (Point2 0 0)

gridValue :: Part -> Point 2 Integer -> Char
gridValue Part1 = \case
  Point2 (-1) 1 -> '1'
  Point2 0 1 -> '2'
  Point2 1 1 -> '3'
  Point2 (-1) 0 -> '4'
  Point2 0 0 -> '5'
  Point2 1 0 -> '6'
  Point2 (-1) (-1) -> '7'
  Point2 0 (-1) -> '8'
  Point2 1 (-1) -> '9'
  _ -> error "not in grid!"
gridValue Part2 = \case
  Point2 0 2 -> '1'
  Point2 (-1) 1 -> '2'
  Point2 0 1 -> '3'
  Point2 1 1 -> '4'
  Point2 (-2) 0 -> '5'
  Point2 (-1) 0 -> '6'
  Point2 0 0 -> '7'
  Point2 1 0 -> '8'
  Point2 2 0 -> '9'
  Point2 (-1) (-1) -> 'A'
  Point2 0 (-1) -> 'B'
  Point2 1 (-1) -> 'C'
  Point2 0 (-2) -> 'D'
  _ -> error "not in grid!"

walkGrid :: Part -> Cardinal -> Ant -> Ant
walkGrid part c a@(antPosition -> v)
  | cardinal 1 c v `elem` gridFor part = shimmy 1 c a
  | otherwise = a

code :: Part -> Point 2 Integer -> [Cardinal] -> Char
code part pos =
  gridValue part . antPosition . foldl' (flip (walkGrid part)) (Ant North pos)

part1 :: String
part1 = map (code Part1 (Point2 0 0)) in02

part2 :: String
part2 = map (code Part2 (Point2 (-2) 0)) in02
