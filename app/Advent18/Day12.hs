module Day12 where

import Data.Set qualified as Set
import Text.Parsec qualified as P

ruleP :: Parser (Map [Bool] Bool)
ruleP = do
  surround <- P.count 5 (oneOf ".#") <* string " => "
  produce <- oneOf ".#"
  pure $ one (('#' ==) <$> surround, '#' == produce)

in12 :: (Set Int, Map [Bool] Bool)
inpots :: Set Int
rules :: Map [Bool] Bool
in12@(inpots, rules) =
  let (in0 :| (_ : text)) = relist $ lines (input 2018 12)
      pots0 =
        foldMap
          (maybe mempty (one . fst) . guarded (('#' ==) . snd))
          . zip [0 ..]
          $ (toString in0 `intersect` ".#")
   in (pots0, foldMap (parse ruleP) text)

step :: Set Int -> Set Int
step s = foldl' go Empty [withNonEmpty 0 minimum1 s - 3 .. withNonEmpty 0 maximum1 s + 3]
  where
    go :: Set Int -> Int -> Set Int
    go m i = case [i - 2 .. i + 2] <&> (`member` s) of
      ls -> case rules !? ls of
        Just k | k -> Set.insert i m
        _ -> Set.delete i m

part1 :: Int
part1 = maybe 0 sum $ iterate' step inpots !!? 20

part2 :: Int
part2 =
  let (length -> a, (b, _) : _) = break (uncurry (==)) . pairwise (,) . pairwise subtract $ sum <$> iterate' step inpots
   in (50000000000 - a) * b + maybe 0 sum (iterate' step inpots !!? a)
