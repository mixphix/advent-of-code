module Advent.Parsers where

import Advent.Coordinates (Cardinal (..))
import Advent.Functions (relist)
import Advent.Numbers (undigits)
import Data.Char (digitToInt)
import Text.Parsec (Parsec, ParsecT, char, digit, many1, oneOf, runParser, runParserT, sepEndBy1, try)
import Text.Parsec qualified

choice :: [Parsec Text u a] -> Parsec Text u a
choice = Text.Parsec.choice . map try

parsedWithStateM :: (Monad m) => ParsecT Text u m a -> u -> Text -> m a
parsedWithStateM p u = either (error . show) id <<$>> runParserT p u ""

parsedWithState :: Parsec Text u a -> u -> Text -> a
parsedWithState p u = either (error . show) id . runParser p u ""

parse :: Parsec Text () a -> Text -> a
parse p = either (error . show) id . Text.Parsec.parse p ""

number :: (Integral n, Monad m) => ParsecT Text u m n
number = do
  signed <- maybe id (const negate) <$> optional (char '-')
  value <- undigits . relist <$> many1 (fromIntegral . digitToInt <$> digit)
  pure $ signed (fromIntegral value)

numberGrid :: (Integral n, Monad m) => ParsecT Text u m [[n]]
numberGrid = numberGridSep (oneOf " ,")

numberGridSep :: (Integral n, Monad m) => ParsecT Text u m a -> ParsecT Text u m [[n]]
numberGridSep p = number `sepEndBy1` many p `sepEndBy1` char '\n'

cardinalP :: Parsec Text u Cardinal
cardinalP =
  choice
    [ North <$ char '^'
    , East <$ char '<'
    , South <$ char 'v'
    , West <$ char '>'
    ]
