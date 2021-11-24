module Advent.Parsers where

import Advent.Coordinates (Cardinal (..))
import Advent.Functions (relist)
import Advent.Numbers (undigits)
import Data.Char (digitToInt)
import Data.Either.Toolbox (keepRight)
import Text.Parsec (Parsec, ParsecT, char, digit, many1, oneOf, parse, runParser, runParserT, sepEndBy1, try)
import Text.Parsec qualified

choice :: [Parsec Text u a] -> Parsec Text u a
choice = Text.Parsec.choice . map try

parsedWithStateM :: (Monad m) => ParsecT Text u m a -> u -> Text -> m (Maybe a)
parsedWithStateM p u = fmap keepRight . runParserT p u ""

parsedWithState :: Parsec Text u a -> u -> Text -> Maybe a
parsedWithState p u = keepRight . runParser p u ""

parsedWith :: Parsec Text () a -> Text -> Maybe a
parsedWith p = keepRight . parse p ""

number :: (Integral n, Monad m) => ParsecT Text u m n
number = do
  signed <- maybe id (const negate) <$> optional (char '-')
  value <- undigits . relist <$> many1 (fromIntegral . digitToInt <$> digit)
  pure $ signed (fromIntegral value)

numberGrid :: (Integral n, Monad m) => ParsecT Text u m [[n]]
numberGrid = numberGridSep (oneOf " ,")

numberGridSep :: (Integral n, Monad m) => ParsecT Text u m a -> ParsecT Text u m [[n]]
numberGridSep p = number `sepEndBy1` many p `sepEndBy1` char '\n'

direction :: Parsec Text u Cardinal
direction =
  choice
    [ North <$ char '^',
      East <$ char '<',
      South <$ char 'v',
      West <$ char '>'
    ]
