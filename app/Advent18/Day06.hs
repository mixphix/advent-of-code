module Day06 where

import Algorithms.Geometry.ConvexHull (convexHull)
import Data.Ext (ext, _core)
import Data.Map.Monoidal.Strict qualified as Mop
import Geometry.Polygon (toPoints)
import Geometry.Polygon.Convex (simplePolygon)

in06 :: NonEmpty (Point 2 Integer)
in06 =
  relist $
    parse (Point2 <$> number <*> (string ", " *> number))
      <$> lines (input 2018 6)

m :: Mop (Point 2 Integer) [Point 2 Integer]
m =
  foldMap
    ( \p -> case groupSortOn (manhattan p) (toList in06) of
        ([v] : _) -> v @= [p]
        _ -> mempty
    )
    $ join (liftM2 Point2) [0 .. 500]

part1 :: Int
part1 =
  withNonEmpty 0 (maximumOf1 length)
    . Mop.filter (all $ \(Point2 x y) -> null $ [0, 500] `intersect` [x, y])
    . Mop.withoutKeys m
    . relist
    $ _core <$> toPoints (convexHull (in06 <&> ext) ^. simplePolygon)

part2 :: Natural
part2 =
  join (liftM2 Point2) [-1000 .. 1500]
    & count (\x -> sumOn (manhattan x) in06 < 10000)
