module Day03 where

in03 :: Natural
in03 = parse number (input 2017 3)

part1 :: Natural
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
