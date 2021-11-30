module Day01 where

directions :: Parser [(Integer, Ant -> Ant)]
directions = choice [l, r] `sepEndBy1` string ", "
  where
    l = do
      n <- char 'L' *> number
      pure (n, turnL)
    r = do
      n <- char 'R' *> number
      pure (n, turnR)

in01 :: [(Integer, Ant -> Ant)]
in01 = parse directions (input 2016 1)

part1 :: Natural
part1 = sumOn (fromIntegral . abs) . antPosition $ foldl' (\ant (n, t) -> scurry n $ t ant) (antCentre North) in01

part2 :: Natural
part2 =
  maybe 0 (sumOn (fromIntegral . abs))
    . firstDuplicate
    . map antPosition
    . relist
    $ foldl'
      ( \ants@(last -> ant) (n, t) ->
          ants ||> genericTake n (drop 1 $ iterate (scurry 1) (t ant))
      )
      (one $ antCentre North)
      in01
