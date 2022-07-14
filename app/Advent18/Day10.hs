module Day10 where

import Data.Set qualified as Set

in10 :: [V 2 (V 2 Integer)]
in10 = (lines (input 2018 10) <&>) . parse $ do
  p <- do
    xcoord <- string "position=<" *> optional (char ' ') *> number
    ycoord <- string ", " *> optional (char ' ') *> number <* char '>'
    pure $ Vector2 xcoord ycoord
  v <- do
    xcoord <- string " velocity=<" *> optional (char ' ') *> number
    ycoord <- string ", " *> optional (char ' ') *> number <* char '>'
    pure $ Vector2 xcoord ycoord
  pure $ Vector2 p v

step :: V 2 (V 2 Integer) -> V 2 (V 2 Integer)
step (Vector2 p v) = Vector2 (p ^+^ v) v

part1 :: [String]
part2 :: Int
(part1, part2) =
  let (p2, vs) =
        (length *** withNonEmpty [] (sort . map (Point . (^. _x)) . head))
          . span ((< 135) . yeas . pairwise nearby . sort)
          $ iterate' (map step) in10
      (Point v, Point ((^-^ v) -> (Vector2 x1 y1))) =
        withNonEmpty (origin, origin) (head &&& last) vs
      stars = relist $ vs <&> (^-^ v) . toVec
      starmap =
        relist
          [ (Point c, if c `Set.member` stars then '#' else ' ')
          | c <- liftM2 Vector2 [-1 .. x1] [-1 .. y1]
          ]
   in (starmap ^. from grid, p2)
 where
  nearby :: V 2 (V 2 Integer) -> V 2 (V 2 Integer) -> Bool
  nearby (Vector2 p _) (Vector2 p' _) = on manhattan Point zero (p' ^-^ p) <= 1
