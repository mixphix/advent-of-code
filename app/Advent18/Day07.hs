module Day07 where

import Data.List.Toolbox qualified as List
import Data.Map.Monoidal.Strict qualified as Mop
import Data.Map.Strict qualified as Map

stepP :: Parser (Mop Char String)
stepP = do
  dep <- string "Step " *> anyChar <* string " must be finished before step "
  suc <- anyChar
  pure $ one (dep, [suc])

in07 :: Mop Char String
in07 = fmap sort . fold . parse stepP <$> lines (input 2018 7)

dependencies :: Mop Char String
dependencies = Mop.foldMapWithKey (\k vs -> relist $ (,[k]) <$> vs) in07

initial :: String
initial = keys in07 \\ keys dependencies

part1 :: String
part1 = go (sort initial) in07 dependencies
  where
    go :: String -> Mop Char String -> Mop Char String -> String
    go [] _ _ = []
    go (x : xs) m d =
      let (fold -> str, m') = popKeys [x] m
          d' = List.delete x <$> d
       in x : go (sort $ xs <> filter (null . (d' !)) str) m' d'

stepTime :: Char -> Natural
stepTime = (61 +) . alphabetPos

tick :: Natural -> Map Char Natural -> Map Char Natural -> Mop Char String -> Mop Char String -> Natural
tick spent Empty _ _ _ = spent
tick spent (fmap pred -> active) waiting yet d =
  let (done, active') = toSnd (Map.withoutKeys active) . Map.keysSet $ Map.filter (== 0) active
      (fold -> str, yet') = popKeys done yet
      d' = d <&> (\\ relist done)
      ready = relist . fmapToSnd stepTime $ filter (null . (d' !)) str
      waiting' = waiting <> ready
      active'' = active' <> Map.take (5 - length active') waiting'
   in tick (succ spent) active'' (waiting' `Map.withoutKeys` Map.keysSet active'') yet' d'

part2 :: Natural
part2 = tick 0 (relist $ fmapToSnd stepTime initial) Empty in07 dependencies
