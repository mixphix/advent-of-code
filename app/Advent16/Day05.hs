module Day05 where

import Control.Monad.Toolbox (loop)
import Crypto.Hash (Digest, MD5, hash)
import Data.Map.Monoidal.Strict qualified as Mop
import Data.Text qualified as T

md5 :: Text -> Text
md5 t = show (hash (encodeUtf8 t :: ByteString) :: Digest MD5)

in05 :: Text
in05 = T.strip $ input 2016 5

hashStartFiveZeros :: Int -> Maybe Text
hashStartFiveZeros = T.stripPrefix "00000" . md5 . (in05 <>) . show

part1 :: String
part1 = take 8 $ mapMaybe (fmap fst . (T.uncons <=< hashStartFiveZeros)) [0 ..]

part2 :: String
part2 =
  mapMaybe getFirst
    . take 8
    . elems
    . loop
      ( \case
          (m, t : ts)
            | take 8 (keys m) == [0 .. 7] -> Right m
            | (p : q : _) <- take 2 (toString t),
              p `elem` ['0' .. '7'] ->
              Left (insert (digitToInt p) (First $ Just q) m, ts)
            | otherwise -> Left (m, ts)
          _ -> error "empty list"
      )
    . (Mop.empty,)
    $ mapMaybe hashStartFiveZeros [0 ..]
