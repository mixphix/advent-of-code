module Day08 where

import Advent
import Control.Lens (from)
import Data.Map.Strict qualified as Map

type Screen = Map (Point 2 Natural) Bool

blank :: Screen
blank = foldr (`insert` False) Empty $ Point2 <$> [0 .. 49] <*> [0 .. 5]

data Command
  = Rect Natural Natural
  | RotateRow Natural Natural
  | RotateCol Natural Natural
  deriving (Show)

command :: Parser Command
command = choice [rect, row, col]
  where
    rect = do
      w <- string "rect " *> number
      h <- char 'x' *> number
      pure $ Rect w h

    row = do
      r <- string "rotate row y=" *> number
      k <- string " by " *> number
      pure $ RotateRow r k

    col = do
      c <- string "rotate column x=" *> number
      k <- string " by " *> number
      pure $ RotateCol c k

runCommand :: Command -> Screen -> Screen
runCommand = \case
  Rect w h -> \m -> foldr (`insert` True) m (Point2 <$> [0 .. pred w] <*> [0 .. pred h])
  RotateRow r k -> \m ->
    let row = relist $ (`Point2` r) <$> [0 .. 49]
        curs = Map.restrictKeys m row
        shift (Point2 x y) = Point2 ((50 + x - k) `mod` 50) y
     in foldr (\v -> alter (const $ curs !? shift v) v) m row
  RotateCol c k -> \m ->
    let col = relist $ Point2 c <$> [0 .. 5]
        curs = Map.restrictKeys m col
        shift (Point2 x y) = Point2 x ((6 + y - k) `mod` 6)
     in foldr (\v -> alter (const $ curs !? shift v) v) m col

in08 :: [Command]
in08 = mapMaybe (parsedWith command) $ lines (input 2016 8)

part1 :: Natural
part1 = yeas $ foldl' (flip runCommand) blank in08

part2 :: [String]
part2 = map (map (\case True -> '#'; _ -> ' ')) $ foldl' (flip runCommand) blank in08 ^. from grid
