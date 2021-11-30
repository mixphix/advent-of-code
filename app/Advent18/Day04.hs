module Day04 where

import Data.List.NonEmpty qualified as NE
import Data.Map.Monoidal.Strict qualified as Mop
import Data.Maybe (fromJust)
import Data.Time.Toolbox

dayP :: Parser Day
dayP = fromJust . parseYmd @Day <$> (char '[' *> many (satisfy (isDigit ||^ (== '-'))) <* char ' ')

minutesOfP :: Day -> Parser [Int]
minutesOfP (ymd -> d) = try $ do
  nsleep <- char '[' *> string d *> string " 00:" *> number <* string "] falls asleep" <* space
  nwake <- char '[' *> string d *> string " 00:" *> number <* string "] wakes up" <* space
  pure [nsleep .. nwake - 1]

guardP :: Parser (Day, Int, [Int])
guardP = do
  day' <- dayP
  hour <- number @Int
  void $ char ':' *> number @Int <* string "] Guard #"
  let day = (if hour == 23 then succ else id) day'
  guardID <- number <* string " begins shift" <* space
  mins <- fmap fold . many $ minutesOfP day
  pure (day, guardID, mins)

in04 :: Mop Int [Int]
in04 =
  foldMap (\(_, grd, mins) -> one (grd, mins))
    . (?: [])
    . parsedWith (many guardP)
    . unlines
    . sort
    $ lines (input 2018 4)

part1 :: Int
part1 =
  let g = Mop.foldlWithKey' (\acc k v -> if length v > length (in04 Mop.! acc) then k else acc) 0 in04
      Just mostmin = viaNonEmpty head <=< viaNonEmpty head . sortOn (Down . length) $ maybe [] groupSort (in04 !? g)
   in g * mostmin

part2 :: Int
part2 =
  let m = fmap (withNonEmpty (0, 0) ((length &&& head) . head) . sortOn (Down . length) . NE.group . sort) in04
      (g, (_, w)) = Mop.foldlWithKey' (\(grd, val) k v -> if on (>) fst v val then (k, v) else (grd, val)) (0, (0, 0)) m
   in g * w
