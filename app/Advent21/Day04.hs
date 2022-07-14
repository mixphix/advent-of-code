module Day04 where

import Data.Text qualified as T

type Bingo = Map (Point 2 Integer) (Natural, Bool)

bingoP :: Parser Bingo
bingoP =
  fmap (fmap (,False) . view grid) $
    (optional (char ' ') *> number `sepBy` many (char ' ')) `sepBy` char '\n'

in04 :: ([Natural], [Bingo])
order :: [Natural]
bingos :: [Bingo]
in04@(order, bingos) =
  let (h : (unlines . drop 1 -> t)) = lines (input 2021 4)
   in ( parse number <$> T.splitOn "," h
      , parse bingoP . (<> "\n") <$> T.splitOn "\n\n" t
      )

winning :: Bingo -> Bool
winning b =
  let rows = map (Point2 <$> [0 .. 4] ??) [0 .. 4]
      cols = map (flip Point2 <$> [0 .. 4] ??) [0 .. 4]
   in any (all (maybe False snd . (b !?))) (rows <> cols)

mark :: Natural -> Bingo -> Bingo
mark n b
  | n `notElem` (fst <$> elems b) = b
  | otherwise = b <&> \(l, p) -> (l, p || l == n)

winner :: [Natural] -> [Bingo] -> (Natural, Bingo)
winner [] _ = error "no winner"
winner _ [] = error "no bingos"
winner (n : ns) bs
  | Just b <- find winning bs' = (n, b)
  | otherwise = winner ns bs'
 where
  bs' = map (mark n) bs

part1 :: Natural
part1 =
  let (n, b) = uncurry winner in04
   in n * sumOn (\(l, p) -> if p then 0 else l) b

loser :: [Natural] -> [Bingo] -> (Natural, Bingo)
loser _ [] = error "no bingos"
loser [] _ = error "no draws"
loser [n] (b : _) = (n, b)
loser (n : ns) bs
  | Just wins <- nonEmpty (filter winning bs') =
    withNonEmpty (n, last wins) (loser ns . toList) (bs' \\ toList wins)
  | otherwise = loser ns bs'
 where
  bs' = map (mark n) bs

part2 :: Natural
part2 =
  let (n, b) = uncurry loser in04
   in n * sumOn (\(l, p) -> if p then 0 else l) b
