module Day15 where

import Advent

type Ingredient = (Integer, V 4 Integer)

ingredient :: Parser (String, Ingredient)
ingredient = do
  name <- many1 alphaNum
  capacity <- string ": capacity " *> number
  durability <- string ", durability " *> number
  flavor <- string ", flavor " *> number
  texture <- string ", texture " *> number
  calories <- string ", calories " *> number
  pure (name, (calories, Vector4 capacity durability flavor texture))

teaspoons :: [[Integer]]
teaspoons = do
  a <- [1 .. 100]
  b <- [1 .. 100 - a]
  c <- [1 .. 100 - (a + b)]
  d <- [1 .. 100 - (a + b + c)]
  sum [a, b, c, d] == 100 ? [a, b, c, d]

in15 :: [Ingredient]
in15 = mapMaybe (snd <<$>> parsedWith ingredient) $ lines (input 2015 15)

cookie :: [Integer] -> (Integer, Integer)
cookie tsps =
  let (cals, ingrs) = unzip $ zipWith (\t (c, i) -> (t * c, t *^ i)) tsps $ take (length tsps) in15
   in (sum cals, productOn (max 0) $ sumV ingrs)

part1 :: Integer
part1 = maximumOf (snd . cookie) teaspoons ?: 0

part2 :: Integer
part2 = maximum $ mapMaybe (snd <<$>> guarded ((500 ==) . fst) . cookie) teaspoons
