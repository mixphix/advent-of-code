module Day20 where

import Advent
import Data.Geometry.Vector qualified as V
import Data.List.Toolbox (groupSort)

particle :: Parser (V 3 (V 3 Integer))
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
  pure $ Vector3 (Vector3 px vx ax) (Vector3 py vy ay) (Vector3 pz vz az)

in20 :: [V 3 (V 3 Integer)]
in20 = mapMaybe (parsedWith particle) $ lines (input 2017 20)

reposition :: Integer -> V 3 (V 3 Integer) -> V 3 (V 3 Integer)
reposition n (Vector3 (Vector3 px vx ax) (Vector3 py vy ay) (Vector3 pz vz az)) =
  Vector3
    (Vector3 (px + (n * vx) + (n * n * ax)) (vx + (n * ax)) ax)
    (Vector3 (py + (n * vy) + (n * n * ay)) (vy + (n * ay)) ay)
    (Vector3 (pz + (n * vz) + (n * n * az)) (vz + (n * az)) az)

part1 :: Natural
part1 = maybe 0 fst . minimumOn (manhattan origin . Point . fmap V.head . reposition 1000000 . snd) $ zip [0 ..] in20

simulate :: Integer -> [V 3 (V 3 Integer)] -> [V 3 (V 3 Integer)]
simulate 0 vs = vs
simulate n (map (reposition 1) . simulate (n - 1) -> vs) =
  let ps = foldMap (relist @_ @Set) . filter ((> 1) . length) . groupSort $ V.head <<$>> vs
   in filter (\v -> fmap V.head v `notMember` ps) vs

part2 :: Int
part2 = length $ simulate 1000 in20
