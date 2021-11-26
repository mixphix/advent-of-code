module Day11 where

import Advent
import Data.Text qualified as T

hexManhattan :: V3 Integer -> V3 Integer -> Integer
hexManhattan v w = maximum $ fmap abs (v - w)

data HexCardinal
  = Up
  | UpLeft
  | UpRight
  | Dn
  | DnLeft
  | DnRight
  deriving (Eq, Show)

hexCardinal :: HexCardinal -> V3 Integer -> V3 Integer
hexCardinal = \case
  UpLeft -> (V3 (-1) 0 1 +)
  Up -> (V3 0 (-1) 1 +)
  UpRight -> (V3 1 (-1) 0 +)
  DnRight -> (V3 1 0 (-1) +)
  Dn -> (V3 0 1 (-1) +)
  DnLeft -> (V3 (-1) 1 0 +)

hexcardinalP :: Parser HexCardinal
hexcardinalP =
  choice
    [ UpLeft <$ string "nw",
      UpRight <$ string "ne",
      Up <$ string "n",
      DnLeft <$ string "sw",
      DnRight <$ string "se",
      Dn <$ string "s"
    ]

in11 :: [HexCardinal]
in11 = mapMaybe (parsedWith hexcardinalP) . T.splitOn "," . withNonEmpty "" head $ lines (input 2017 11)

part1 :: Integer
part1 = hexManhattan (V3 0 0 0) $ foldl' (flip hexCardinal) (V3 0 0 0) in11

part2 :: Integer
part2 = maximumOf (hexManhattan (V3 0 0 0)) (scanl' (flip hexCardinal) (V3 0 0 0) in11) ?: 0
