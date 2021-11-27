module Day19 where

import Advent
import Data.Map.Monoidal.Strict qualified as Mop
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

reactionRule :: Parser (Text, [Text])
reactionRule =
  (,) <$> (toText <$> many1 alphaNum <* string " => ") <*> (one . toText <$> many1 alphaNum)

molecules :: String -> Seq Text
molecules (m1 : m2 : ms)
  | isUpper m1 && isUpper m2 = one m1 :<| molecules (m2 : ms)
  | isUpper m1 && isLower m2 = toText [m1, m2] :<| molecules ms
  | otherwise = error "m1 is lowercase"
molecules [x] = one (one x)
molecules "" = Empty

in19 :: (Mop Text [Text], Seq Text)
rules :: Mop Text [Text]
medicine :: Seq Text
in19@(rules, medicine) =
  let text = lines $ input 2015 19
      rrs = Mop.fromList $ mapMaybe (parsedWith reactionRule) text
      med = withNonEmpty "" (toString . last) text
   in (rrs, molecules med)

inverseRules :: Mop Text Text
inverseRules = Mop.foldMapWithKey (\k vs -> fromList $ (,k) <$> vs) rules

part1 :: Natural
part1 =
  count every . flip Seq.foldMapWithIndex medicine $ \i ->
    maybe Set.empty (foldMap (\y -> one . fold $ Seq.adjust (const y) i medicine)) . (rules !?)

part2 :: Int
part2 =
  length medicine
    - length (filter (`elem` ["Rn", "Ar"]) med)
    - 2 * length (filter ("Y" ==) med)
    - 1
  where
    med = relist medicine
