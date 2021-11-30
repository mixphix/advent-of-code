module Day04 where

import Data.List.NonEmpty.Toolbox qualified as NE
import Data.Text qualified as T

decoy :: Text -> Bool
decoy t =
  let checksum = T.dropWhile (/= '[') $ T.dropWhileEnd (/= ']') t
      tstr = toString $ T.filter isAlpha (T.dropWhileEnd (/= '[') t)
      (a : b : c : d : e : _) =
        map head . sortBy (comparing (Down . length) <> comparing head) . NE.group $ sort tstr
   in foldMap one ['[', a, b, c, d, e, ']'] /= checksum

sectorID :: Text -> Natural
sectorID = parse number . T.filter isDigit

in04 :: [Text]
in04 = lines (input 2016 4)

part1 :: Natural
part1 = sumOn sectorID $ filter (not . decoy) in04

decrypt :: Text -> (Natural, Text)
decrypt t =
  let n = sectorID t
   in (n,) . strip . flip T.map (T.dropWhileEnd (/= '[') t) $ \case
        '-' -> ' '
        '[' -> ' '
        ']' -> ' '
        c
          | isDigit c -> ' '
          | otherwise ->
            let p = (alphabetPos c + n) `mod` 26
             in ['a' .. 'z'] !!? fromIntegral p ?: '+'

part2 :: Natural
part2 =
  let decrypted = map decrypt in04
   in withNonEmpty 0 (fst . head) $ filter (T.isInfixOf "northpole" . snd) decrypted
