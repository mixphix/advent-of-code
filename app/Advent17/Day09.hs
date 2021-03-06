module Day09 where

garbageP :: Parser Int
garbageP = do
  void $ string "<"
  k <- many (choice [0 <$ (string "!" >> anyChar), 1 <$ satisfy (/= '>')])
  sum k <$ string ">"

groupP :: Int -> Parser Int
groupP s = between (string "{") (string "}") $ do
  subscores <- choice [groupP (succ s), 0 <$ garbageP] `sepBy` string ","
  pure $ s + sum subscores

part1 :: Int
part1 = parse (groupP 1) (input 2017 9)

removeGarbageP :: Parser Int
removeGarbageP = sum <$> many (choice [garbageP, 0 <$ anyChar])

part2 :: Int
part2 = parse removeGarbageP (input 2017 9)
