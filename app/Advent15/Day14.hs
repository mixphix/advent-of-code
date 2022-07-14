module Day14 where

import Data.Map.Strict qualified as Map

type Speed = Integer

type Duration = Integer

type Rest = Integer

type Roster = Map String (Speed, Duration, Rest)

roster :: Parser (String, (Speed, Duration, Rest))
roster = do
  reindeer <- many1 alphaNum <* string " can fly "
  speed <- number <* string " km/s for "
  duration <- number <* string " seconds, but then must rest for "
  rest <- number
  pure (reindeer, (speed, duration, rest))

in14 :: Roster
in14 = relist $ parse roster <$> lines (input 2015 14)

race :: Duration -> String -> Integer
race time name =
  let Just (speed, flydur, restdur) = in14 !? name
      (q, r) = time `quotRem` (flydur + restdur)
   in speed * flydur * q + speed * min r flydur

part1 :: Integer
part1 = withNonEmpty 0 (maximumOf1 (race 2503)) (keys in14)

type Points = Integer

type Racing = Map String (Points, Integer)

points :: Duration -> Points
points time = go (Map.fromSet (const (-1, 0)) (Map.keysSet in14)) time
 where
  go :: Racing -> Duration -> Points
  go m 0 = withNonEmpty 0 (fst . maximum1) m
  go m n = go m' (pred n)
   where
    elapsed = time - n
    distances = Map.fromSet (race elapsed) (Map.keysSet in14)
    m' =
      Map.adjust
        (first succ)
        (withNonEmpty "" (fst . maximumOn1 snd) (Map.assocs distances))
        m

part2 :: Integer
part2 = points 2503
