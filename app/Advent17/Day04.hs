module Day04 where

in04 :: [Text]
in04 = lines (input 2017 4)

part1 :: Natural
part1 = count (not . anySame . words) in04

part2 :: Natural
part2 = count (noAnagrams . words) in04
 where
  noAnagrams :: [Text] -> Bool
  noAnagrams [] = True
  noAnagrams (x : xs) = all (`notElem` perms x) xs && noAnagrams xs

  perms :: Text -> [Text]
  perms = map toText . permutations . toString
