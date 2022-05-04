{-# LANGUAGE FlexibleContexts #-}

module Day11 where

import Data.Map.NonEmpty qualified as NEMap

in11 :: Integer
in11 = parse number (input 2018 11)

powerlevel :: Point 2 Integer -> Integer
powerlevel (Point (Vector2 x y)) =
  let rackID = x + 10
   in subtract 5 $ (rackID * ((rackID * y) + in11) `mod` 1000) `div` 100

fuelcells :: NEMop (Point 2 Integer) (Sum Integer)
fuelcells =
  NEMop . NEMap.fromSet (Sum . powerlevel) . relist $ join (liftM2 Point2) [1 .. 300]

cumulcells :: NEMop (Point 2 Integer) (Sum Integer)
cumulcells =
  relist @(Map (Point 2 Integer)) @(NEMop (Point 2 Integer)) . foldl' f Empty $ join (liftM2 Point2) [1 .. 300]
 where
  f m p@(Point2 x y) =
    insert p (Sum (powerlevel p) + m ! Point2 (x - 1) y + m ! Point2 x (y - 1) - m ! Point2 (x - 1) (y - 1)) m

subcell :: Integer -> Point 2 Integer -> Integer
subcell w (Point2 x0 y0) =
  let x = x0 - 1
      y = y0 - 1
      x' = x + w
      y' = y + w
      [a, b, c, d] = getSum . (cumulcells !) <$> [Point2 x y, Point2 x y', Point2 x' y, Point2 x' y']
   in d - b - c + a

maxsubcumul :: Integer -> (Point 2 Integer, Integer)
maxsubcumul w =
  withNonEmpty (Point2 0 0, 0) (maximumOn1 snd) [(p, subcell w p) | p <- join (liftM2 Point2) [1 .. 301 - w]]

part1 :: (Integer, Integer)
part1 =
  let totalpower pt = getSum $ surrounding 1 pt >-< (fuelcells !)
   in withNonEmpty (0, 0) (join (***) pred . view pair . maximumOn1 totalpower) $ join (liftM2 Point2) [2 .. 299]

part2 :: (Integer, Integer, Integer)
part2 =
  withNonEmpty (0, 0, 0) (fst . maximumOn1 snd) [((x, y, w), l) | w <- [3 .. 300], let (Point2 x y, l) = maxsubcumul w]
