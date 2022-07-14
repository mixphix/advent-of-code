module Day05 where

lineP :: Parser [Point 2 Integer]
lineP = do
  s@(Point start) <-
    Point2 <$> (number <* char ',') <*> (number <* string " -> ")
  e@(Point end) <-
    Point2 <$> (number <* char ',') <*> number
  let d = (end ^-^ start) <&> \c -> if c >= 0 then min 1 c else max (-1) c
  pure . (<> [e]) . takeWhile (/= e) $
    iterate' (\(Point v) -> Point (v ^+^ d)) s

straightline :: [Point 2 Integer] -> Bool
straightline = allSame . map (view xCoord) ||^ allSame . map (view yCoord)

in05 :: [[Point 2 Integer]]
in05 = parse lineP <$> lines (input 2021 5)

part1 :: Natural
part1 =
  count @(Mop _) (> 1) $
    filter straightline in05 >-< foldMap (@= (1 :: Sum Natural))

part2 :: Natural
part2 =
  count @(Mop _) (> 1) $
    in05 >-< foldMap (@= (1 :: Sum Natural))
