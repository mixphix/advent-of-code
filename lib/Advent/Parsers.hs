module Advent.Parsers where

import Advent.Coordinates (Cardinal (..))
import Advent.Functions (relist)
import Advent.Numbers (undigits)
import Data.Char (digitToInt)
import Data.Either.Toolbox (keepRight)
import Text.Parsec (char, digit, many1, oneOf, parse, sepEndBy1, try)
import Text.Parsec qualified
import Text.Parsec.Text (Parser)

choice :: [Parser a] -> Parser a
choice = Text.Parsec.choice . map try

parsedWith :: Parser a -> Text -> Maybe a
parsedWith p = keepRight . parse p ""

number :: (Integral n) => Parser n
number = do
  signed <- maybe id (const negate) <$> optional (char '-')
  value <- undigits . relist <$> many1 (fromIntegral . digitToInt <$> digit)
  pure $ signed (fromIntegral value)

numberGrid :: (Integral n) => Parser [[n]]
numberGrid = numberGridSep (oneOf " ,")

numberGridSep :: (Integral n) => Parser a -> Parser [[n]]
numberGridSep p = number `sepEndBy1` many p `sepEndBy1` char '\n'

direction :: Parser Cardinal
direction =
  choice
    [ North <$ char '^',
      East <$ char '<',
      South <$ char 'v',
      West <$ char '>'
    ]
