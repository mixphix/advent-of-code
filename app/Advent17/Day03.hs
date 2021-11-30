module Day03 where

in03 :: Natural
in03 = parsedWith number (input 2017 3) ?: 0

part1 :: Integer
part1 = manhattan origin $ in03 ^. ulam

part2 :: Natural
part2 = f 2 $ one (view ulam 1, 1)
  where
    f :: Natural -> Map (Point 2 Integer) Natural -> Natural
    f n@(view ulam -> v) g
      | write > in03 = write
      | otherwise = f (succ n) $! alter (const $ pure write) v g
      where
        write = sum $ mapMaybe (g !?) (moore v)
