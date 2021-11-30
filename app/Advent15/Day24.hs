module Day24 where

in24 :: [Natural]
weight :: Natural
(in24, weight) = (id &&& sum) $ parse number <$> lines (input 2015 24)

footsacks :: [(Int, Natural)]
footsacks = takeWhile ((smallestSizeOver wgt ==) . fst) $ do
  a <- subsequences in24
  guard $ equal a
  let in24noa = in24 \\ a
  b <- subsequences in24noa
  guard $ equal b
  let [x, _, _] = sortOn length [a, b, in24noa \\ b]
  pure $ (length &&& product) x
  where
    wgt = weight `div` 3
    equal x = sum x == wgt

smallestSizeOver :: Natural -> Int
smallestSizeOver n = succ . genericLength . takeWhile ((n >=) . sum) $ inits (reverse in24)

part1 :: Natural
part1 = withNonEmpty 0 (minimumOf1 snd) footsacks

trunksacks :: [(Int, Natural)]
trunksacks = takeWhile ((smallestSizeOver wgt ==) . fst) $ do
  a <- subsequences in24
  guard $ equal a
  let in24noa = in24 \\ a
  b <- subsequences in24noa
  guard $ equal b
  let in24noab = in24noa \\ b
  c <- subsequences in24noab
  guard $ equal c
  let [x, _, _, _] = sortOn length [a, b, c, in24noab \\ c]
  pure $ (length &&& product) x
  where
    wgt = weight `div` 4
    equal x = sum x == wgt

part2 :: Natural
part2 = withNonEmpty 0 (minimumOf1 snd) trunksacks
