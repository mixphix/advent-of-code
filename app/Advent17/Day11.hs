module Day11 where

import Advent
import Data.Text qualified as T

hexManhattan :: V 3 Integer -> V 3 Integer -> Integer
hexManhattan v w = maximum $ fmap abs (v ^-^ w)

data HexCardinal
  = Up
  | UpLeft
  | UpRight
  | Dn
  | DnLeft
  | DnRight
  deriving (Eq, Show)

hexCardinal :: HexCardinal -> V 3 Integer -> V 3 Integer
hexCardinal = \case
  UpLeft -> (Vector3 (-1) 0 1 ^+^)
  Up -> (Vector3 0 (-1) 1 ^+^)
  UpRight -> (Vector3 1 (-1) 0 ^+^)
  DnRight -> (Vector3 1 0 (-1) ^+^)
  Dn -> (Vector3 0 1 (-1) ^+^)
  DnLeft -> (Vector3 (-1) 1 0 ^+^)

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
part1 = hexManhattan (Vector3 0 0 0) $ foldl' (flip hexCardinal) (Vector3 0 0 0) in11

part2 :: Integer
part2 = maximumOf (hexManhattan (Vector3 0 0 0)) (scanl' (flip hexCardinal) (Vector3 0 0 0) in11) ?: 0
