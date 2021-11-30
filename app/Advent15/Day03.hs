module Day03 where

in03 :: [Cardinal]
in03 = parsedWith (many cardinalP) (input 2015 3) ?: []

type Visits = Mop (Point 2 Integer) (Sum Int)

move :: (Ant, Visits) -> Cardinal -> (Ant, Visits)
move (a, m) c = (a', insert (antPosition a') (Sum 1) m)
  where
    a' = shimmy 1 c a

start :: Visits
start = one (Point2 0 0, Sum 1)

deliver :: [Cardinal] -> Visits -> Visits
deliver cs m = snd $ foldl' move (antCentre North, m) cs

part1 :: Natural
part1 = count (>= Sum 1) $ deliver in03 start

part2 :: Natural
part2 = count (>= Sum 1) . deliver robo $ deliver santa start
  where
    (santa, robo) = uninterleave in03
