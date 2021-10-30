module Day07 where

import Advent
import Data.List (intersect)

data IPv7Part
  = Hypernet Text
  | Supernet Text
  deriving (Show)

type IPv7 = [IPv7Part]

ipv7 :: Parser IPv7
ipv7 = many (asum [Supernet <$> reg, Hypernet <$> hyper])
  where
    hyper = fmap toText $ char '[' *> many1 lower <* char ']'
    reg = toText <$> many1 lower

in07 :: [IPv7]
in07 = mapMaybe (parsedWith ipv7) $ lines (input 2016 7)

tls :: IPv7 -> Bool
tls ip = none abba [toString t | Hypernet t <- ip] && any abba [toString t | Supernet t <- ip]
  where
    abba :: String -> Bool
    abba (w : x : y : z : xs) = w == z && x == y && w /= x || abba (x : y : z : xs)
    abba _ = False

part1 :: Natural
part1 = count tls in07

ssl :: IPv7 -> Bool
ssl ip = case aba =<< [toString t | Supernet t <- ip] of
  abas -> case aba =<< [toString t | Hypernet t <- ip] of
    babs -> notNull $ map swap abas `intersect` babs
  where
    aba :: String -> [(Char, Char)]
    aba (x : y : z : xs)
      | x == z && x /= y = (x, y) : aba (y : z : xs)
      | otherwise = aba (y : z : xs)
    aba _ = []

part2 :: Natural
part2 = count ssl in07
