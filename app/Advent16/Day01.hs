module Day01 where

import Advent

directions :: Parser [(Natural, Ant -> Ant)]
directions = choice [left, right] `sepEndBy1` string ", "
  where
    left = do
      n <- char 'L' *> number
      pure (n, turnL)
    right = do
      n <- char 'R' *> number
      pure (n, turnR)

in01 :: [(Natural, Ant -> Ant)]
in01 = parsedWith directions (input 2016 1) ?: []

part1 :: Natural
part1 = sumOn (fromIntegral . abs) . antPosV $ foldl' (\ant (n, t) -> scurry n $ t ant) (antCentre North) in01

part2 :: Natural
part2 =
  maybe 0 (sumOn (fromIntegral . abs))
    . firstDuplicate
    . map antPosV
    . relist
    $ foldl'
      ( \ants@(last -> ant) (n, t) ->
          ants ||> genericTake n (drop 1 $ iterate (scurry 1) (t ant))
      )
      (one $ antCentre North)
      in01
