module Day03 where

import Data.Map.Monoidal.Strict qualified as Mop

claimP :: Parser (Int, [Point 2 Natural])
claimP = do
  ident <- char '#' *> number
  Point2 x y <- Point2 <$> (string " @ " *> number) <*> (char ',' *> number)
  Point2 w h <- Point2 <$> (string ": " *> number) <*> (char 'x' *> number)
  pure . (ident,) $ Point2 <$> [x .. x + w - 1] <*> [y .. y + h - 1]

in03 :: Mop Int [Point 2 Natural]
in03 = relist $ parse claimP <$> lines (input 2018 3)

inv :: Mop (Point 2 Natural) [Int]
inv = Mop.foldMapWithKey (\k vs -> relist $ (,[k]) <$> vs) in03

part1 :: Natural
part1 = count ((> 1) . length) inv

part2 :: Int
part2 = withNonEmpty 0 head . keys $ Mop.filter (all ((== 1) . length . (?: []) . (inv !?))) in03
