module Day11 where

import Advent
import Data.Containers.ListUtils (nubOrd)
import Data.List.Toolbox (allSame, intersect)
import Data.Text (strip)

data Lowercase = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Ord, Enum, Bounded)

lowercase :: Lowercase -> Char
lowercase l = ['a' .. 'z'] !!? (l ^. enum) ?: '\0'

type Password = NonEmpty Lowercase

incr :: Password -> Password
incr (Z :| word) = withNonEmpty (:| []) (\w -> (<| incr w)) word A
incr (l :| word) = succ l :| word

in11 :: Password
in11 = toPassword . toString $ strip (input 2015 11)

toPassword :: String -> Password
toPassword = relist . reverse . mapMaybe (inverseMap lowercase)

fromPassword :: Password -> String
fromPassword = map lowercase . reverse . relist

valid :: Password -> Bool
valid (relist -> p) =
  null (p `intersect` [I, O, L])
    && hasMoreThan 2 ((2 ==) . length) pairs
    && hasMoreThan 1 straight (triples p)
  where
    triples (x : y : z : xs) = [x, y, z] : triples (y : z : xs)
    triples _ = []
    straight xs = withNonEmpty False (\(l :| _) -> l /= A && xs == take (length xs) [l, pred l ..]) xs
    pairs = nubOrd . filter allSame $ pairwise list2 p

part1 :: String
part1 = maybe "" fromPassword $ find valid (iterate incr in11)

part2 :: String
part2 = maybe "" fromPassword . find valid $ iterate incr (incr $ toPassword part1)
