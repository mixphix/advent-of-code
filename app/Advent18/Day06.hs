module Day06 where

import Algorithms.Geometry.ConvexHull (convexHull)
import Data.Ext (ext, _core)
import Data.Geometry.Polygon (toPoints)
import Data.Geometry.Polygon.Convex (simplePolygon)
import Data.Map.Monoidal.Strict qualified as Mop

in06 :: NonEmpty (Point 2 Integer)
in06 = relist $ parse (Point2 <$> number <*> (string ", " *> number)) <$> lines (input 2018 6)

m :: Mop (Point 2 Integer) [Point 2 Integer]
m =
  foldMap (\p -> case groupSortOn (manhattan p) (toList in06) of ([v] : _) -> one (v, [p]); _ -> mempty) $
    join (liftM2 Point2) [0 .. 500]

part1 :: Int
part1 =
  withNonEmpty 0 (maximumOf1 length)
    . Mop.filter (none $ ((`elem` [0, 500]) . view xCoord) ||^ ((`elem` [0, 500]) . view yCoord))
    $ m `Mop.withoutKeys` relist (_core <$> toPoints (convexHull (in06 <&> ext) ^. simplePolygon))

part2 :: Natural
part2 = join (liftM2 Point2) [-1000 .. 1500] & count (\x -> sumOn (manhattan x) in06 < 10000)
