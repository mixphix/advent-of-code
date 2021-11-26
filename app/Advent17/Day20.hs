module Day20 where

import Advent
import Data.List.Toolbox (groupSort)

particle :: Parser (V3 (V3 Integer))
particle = do
  px <- string "p=<" *> (many space *> number) <* string ","
  py <- number <* string ","
  pz <- number <* string ">, "
  vx <- string "v=<" *> (many space *> number) <* string ","
  vy <- number <* string ","
  vz <- number <* string ">, "
  ax <- string "a=<" *> (many space *> number) <* string ","
  ay <- (many space *> number) <* string ","
  az <- (many space *> number) <* string ">"
  pure $ V3 (V3 px vx ax) (V3 py vy ay) (V3 pz vz az)

in20 :: [V3 (V3 Integer)]
in20 = mapMaybe (parsedWith particle) $ lines (input 2017 20)

reposition :: Integer -> V3 (V3 Integer) -> V3 (V3 Integer)
reposition n (V3 (V3 px vx ax) (V3 py vy ay) (V3 pz vz az)) =
  V3
    (V3 (px + (n * vx) + (n * n * ax)) (vx + (n * ax)) ax)
    (V3 (py + (n * vy) + (n * n * ay)) (vy + (n * ay)) ay)
    (V3 (pz + (n * vz) + (n * n * az)) (vz + (n * az)) az)

part1 :: Natural
part1 = maybe 0 fst . minimumOn (sumOn abs . fmap (^. _x) . reposition 1000000 . snd) $ zip [0 ..] in20

simulate :: Integer -> [V3 (V3 Integer)] -> [V3 (V3 Integer)]
simulate 0 vs = vs
simulate n (map (reposition 1) . simulate (n - 1) -> vs) =
  let ps = foldMap (relist @_ @Set) . filter ((> 1) . length) . groupSort $ map (fmap (view _x)) vs
   in filter (\v -> fmap (view _x) v `notMember` ps) vs

part2 :: Int
part2 = length $ simulate 1000 in20
