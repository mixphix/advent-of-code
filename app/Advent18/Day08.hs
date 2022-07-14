module Day08 where

import Data.Tree
import Text.Parsec qualified as P

treeP :: Parser (Tree [Int])
treeP = do
  children <- number <* char ' '
  metadata <- number <* char ' '
  subtree <- P.count children treeP
  entries <- P.count metadata (number <* choice [void $ char ' ', eof])
  pure $ Node entries subtree

in08 :: Tree [Int]
in08 = parse treeP (input 2018 8)

part1 :: Int
part1 = foldTree (on (+) sum) in08

part2 :: Int
part2 = flip foldTree in08 $ \as bss -> case bss of
  [] -> sum as
  _ -> sum (mapMaybe ((bss !!?) . pred) as)
