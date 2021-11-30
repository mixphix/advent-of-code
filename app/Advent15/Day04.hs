module Day04 where

import Crypto.Hash (Digest, MD5, hash)
import Data.Text qualified as T

md5 :: Text -> Text
md5 t = show (hash (encodeUtf8 t :: ByteString) :: Digest MD5)

in04 :: Text
in04 = T.strip $ input 2015 4

part1 :: Natural
part1 = find (T.isPrefixOf "00000" . md5 . (in04 <>) . show) [1 ..] ?: 0

part2 :: Natural
part2 = find (T.isPrefixOf "000000" . md5 . (in04 <>) . show) [1 ..] ?: 0
