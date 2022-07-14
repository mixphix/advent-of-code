module Day12 where

import Control.Lens (transform)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (_Object)
import Data.HashMap.Strict qualified as H

in12 :: Value
in12 = Aeson.decodeStrict (encodeUtf8 (input 2015 12)) ?: error "hmm"

numbers :: Value -> [Integer]
numbers = parse (optional p *> number `sepEndBy1` p) . decodeUtf8 . Aeson.encode
 where
  p = many (oneOf " ,:\"{}[]" <|> letter)

noReds :: Value -> Value
noReds =
  transform $
    _Object %~ \m ->
      if Aeson.String "red" `elem` m then H.empty else m

part1 :: Integer
part1 = sum $ numbers in12

part2 :: Integer
part2 = sum $ numbers (noReds in12)
