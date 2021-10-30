module Day09 where

import Advent
import Data.Text qualified as T
import Text.Parsec qualified as P

data Marker
  = NoMarker String
  | Repeated Natural [Marker]
  deriving (Show)

marker :: Part -> Parser [Marker]
marker Part1 = many1 $ choice [repeated, nomarker]
  where
    repeated = do
      chars <- char '(' *> number
      n <- char 'x' *> number <* char ')'
      Repeated n . pure . NoMarker <$> P.count chars anyChar
    nomarker = NoMarker <$> many1 (satisfy (/= '('))
marker Part2 = many1 $ choice [repeated, nomarker]
  where
    repeated = do
      chars <- char '(' *> number
      n <- char 'x' *> number <* char ')'
      t <- toText <$> P.count chars anyChar
      pure . Repeated n $ parsedWith (marker Part2) t ?: []
    nomarker = NoMarker <$> many1 (satisfy (/= '('))

in09 :: Part -> [Marker]
in09 part = parsedWith (marker part) (T.strip $ input 2016 9) ?: []

markerLength :: Marker -> Natural
markerLength = \case
  NoMarker s -> genericLength s
  Repeated n m -> n * sumOn markerLength m

part1 :: Natural
part1 = sumOn markerLength (in09 Part1)

part2 :: Natural
part2 = sumOn markerLength (in09 Part2)
