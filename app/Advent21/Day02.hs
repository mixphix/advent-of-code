module Day02 where

data Command
  = Forward Integer
  | Downward Integer
  | Upward Integer
  deriving (Eq, Ord, Show)

commandP :: Parser Command
commandP =
  choice
    [ Forward <$> (string "forward " *> number),
      Downward <$> (string "down " *> number),
      Upward <$> (string "up " *> number)
    ]

in02 :: [Command]
in02 = parse commandP <$> lines (input 2021 2)

move :: Command -> Point 2 Integer -> Point 2 Integer
move = \case
  Forward k -> \(Point2 x y) -> Point2 (x + k) y
  Downward k -> \(Point2 x y) -> Point2 x (y + k)
  Upward k -> \(Point2 x y) -> Point2 x (y - k)

part1 :: Integer
part1 = product $ foldl' (flip move) (Point2 0 0) in02

aim :: Command -> Point 3 Integer -> Point 3 Integer
aim = \case
  Forward k -> \(Point3 x y a) -> Point3 (x + k) (y + (a * k)) a
  Downward k -> \(Point3 x y a) -> Point3 x y (a + k)
  Upward k -> \(Point3 x y a) -> Point3 x y (a - k)

part2 :: Integer
part2 = foldl' (flip aim) (Point3 0 0 0) in02 & \(Point3 x y _) -> x * y
