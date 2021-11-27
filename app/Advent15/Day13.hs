module Day13 where

import Advent
import Data.List.NonEmpty.Toolbox (groupWith, maximumOf1)

type Table = Mop String (Mop String (Sum Integer))

table :: Parser (String, (String, Sum Integer))
table = do
  i <- many1 alphaNum <* string " would "
  sign <- choice [id <$ string "gain ", negate <$ string "lose "]
  units <- number <* string " happiness units by sitting next to "
  u <- many1 alphaNum
  pure (i, (u, sign units))

in13 :: Table
in13 =
  relist
    . map (fst . head &&& foldMap (one . snd))
    . groupWith fst
    . sort
    . foldMap (maybe [] pure . parsedWith table)
    $ lines (input 2015 13)

names :: NonEmpty String
names = relist $ keys in13

happiness :: NonEmpty String -> Integer
happiness lx@(x :| _) = sum $ pairwise happy xs
  where
    happy p q = getSum $ in13 ! p ! q <> in13 ! q ! p
    xs = relist (lx |> x)

part1 :: Integer
part1 = maximumOf1 happiness (permutationsNE names)

part2 :: Integer
part2 = maximumOf1 happiness (permutationsNE $ "Melanie" <| names)
