module Day10 where

import Data.Text qualified as T
import Data.Vector.Unboxed qualified as V

twist :: Int -> Int -> V.Vector Word8 -> V.Vector Word8
twist pos len v = V.fromList $ fromIntegral <$> twisted
 where
  l = reverse $ [pos .. pos + len - 1] <&> (`mod` V.length v)
  twisted = [0 .. V.length v - 1] <&> \n -> (v V.!) $ elemIndex n l >>= (l !!?) . (pred (length l) -) ?: n

prepASCII :: Text -> [Word8]
prepASCII = (<> [17, 31, 73, 47, 23]) . map (fromIntegral . ord) . toString . strip

in10 :: Part -> [Word8]
in10 part = ($ input 2017 10) $ case part of
  Part1 -> map (parse number) . T.splitOn ","
  Part2 -> prepASCII

runTwists :: Int -> Int -> [Word8] -> V.Vector Word8 -> V.Vector Word8
runTwists _ _ [] v = v
runTwists pos skip (len : lens) v = runTwists ((pos + fromIntegral len + skip) `mod` V.length v) (succ skip) lens (twist pos (fromIntegral len) v)

part1 :: Int
part1 =
  let v = runTwists 0 0 (in10 Part1) (V.fromList [0 .. 255])
   in fromIntegral (v V.! 0) * fromIntegral (v V.! 1)

sparseHash :: [Word8] -> V.Vector Word8
sparseHash l = runTwists 0 0 (fold $ replicate 64 l) $ relist [0 .. 255]

denseHash :: [Word8] -> [Word8]
denseHash = map (foldr xor 0) . chunksOf 16 . V.toList . sparseHash

hexa :: Word8 -> String
hexa n =
  let (q, r) = n `divMod` 16
   in mapMaybe (((['0' .. '9'] <> "abcdef") !!?) . fromIntegral) [q, r]

knotHash :: Text -> [Word8]
knotHash = denseHash . prepASCII

hashString :: [Word8] -> String
hashString = foldMap hexa . denseHash

part2 :: String
part2 = hashString (in10 Part2)
