module Day06 where

type Lights = Map (Point 2 Natural) Integer

lights :: Lights
lights = relist $ (,0) <$> join (liftM2 Point2) [0 .. 999]

zone :: (Maybe Integer -> Integer) -> Point 2 Natural -> Point 2 Natural -> Lights -> Lights
zone f (Point2 l b) (Point2 r t) m = foldr (alter $ Just . f) m $ liftM2 Point2 [l .. r] [b .. t]

toggle, turnOn, turnOff :: Bool -> Point 2 Natural -> Point 2 Natural -> Lights -> Lights
toggle b = zone (if b then maybe 2 (+ 2) else maybe 1 (1 -))
turnOn b = zone (if b then maybe 1 (+ 1) else const 1)
turnOff b = zone (if b then maybe 0 (max 0 . subtract 1) else const 0)

instruction :: Bool -> Parser (Lights -> Lights)
instruction b = do
  f <-
    choice . map try $
      [ toggle b <$ string "toggle ",
        turnOn b <$ string "turn on ",
        turnOff b <$ string "turn off "
      ]
  topl <- liftM2 Point2 (number <* char ',') number <* string " through "
  botr <- liftM2 Point2 (number <* char ',') number
  pure $ f topl botr

in06 :: Bool -> [Lights -> Lights]
in06 b = mapMaybe (parsedWith (instruction b)) $ lines (input 2015 6)

part1 :: Natural
part1 = count (>= 1) $ foldl' (&) lights (in06 False)

part2 :: Integer
part2 = sum $ foldl' (&) lights (in06 True)
