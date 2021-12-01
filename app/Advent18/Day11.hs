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
  NEMop
    . NEMap.fromSet (Sum . (\(Point2 x y) -> sumOn powerlevel $ liftM2 Point2 [1 .. x] [1 .. y]))
    . relist
    $ join (liftM2 Point2) [1 .. 300]

part1 :: (Integer, Integer)
part1 =
  let totalpower pt = getSum $ surrounding 1 pt >-< (fuelcells !)
   in withNonEmpty (0, 0) (join (***) pred . view pair . maximumOn1 totalpower) $ join (liftM2 Point2) [2 .. 299]

part2 :: (Integer, Integer, Integer)
part2 =
  let totalpower w p@(Point2 x y) =
        let [a, b, c, d] = getSum . (cumulcells !) <$> [Point2 (x - w) (y - w), Point2 x (y - w), Point2 (x - w) y, p]
         in a + d - (b + c)
   in withNonEmpty (0, 0, 0) (fst . maximumOn1 snd) $
        [ ((x, y, w), totalpower w p)
          | w@(fromIntegral -> w') <- [1 .. 300],
            p@(Point2 x y) <- join (liftM2 Point2) [succ w' .. 300 - succ w']
        ]
