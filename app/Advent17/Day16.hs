module Day16 where

import Data.Text qualified as T
import Data.Vector (backpermute)
import Data.Vector qualified as V
import Data.Vector.Fixed qualified as VF
import Data.Vector.Fixed.Unboxed qualified as VFU

data Dance
  = Spin Int
  | Exchange Int Int
  | Partner Char Char
  deriving (Eq, Show)

danceP :: Parser Dance
danceP = choice [spin, exchange, partner]
  where
    spin = Spin <$> (char 's' *> number)
    exchange = do
      p0 <- char 'x' *> number
      p1 <- char '/' *> number
      pure $ Exchange p0 p1
    partner = do
      p0 <- char 'p' *> oneOf ['a' .. 'p']
      p1 <- char '/' *> oneOf ['a' .. 'p']
      pure $ Partner p0 p1

newtype Permutation = Permutation (VFU.Vec 16 Int)
  deriving (Eq, Ord, Show)

instance Semigroup Permutation where
  Permutation v <> Permutation w =
    Permutation . VF.fromList . relist $ (backpermute `on` relist . VF.toList) w v

instance Monoid Permutation where
  mempty = Permutation $ VF.fromList [0 .. 15]

in16 :: [Dance]
in16 = mapMaybe (parsedWith danceP) $ T.splitOn "," (input 2017 16)

toPerm :: Vector Char -> Permutation
toPerm = Permutation . VF.fromList . mapMaybe (`elemIndex` ['a' .. 'p']) . relist

dance :: Dance -> Vector Char -> Vector Char
dance (Spin k) dancers = let (front, back) = splitAt (16 - k) (relist dancers) in relist $ back <> front
dance (Exchange i j) dancers =
  let [m, n] = map (dancers V.!) [i, j]
   in dancers & ix i .~ n & ix j .~ m
dance (Partner m n) dancers =
  let [i, j] = mapMaybe (`V.elemIndex` dancers) [m, n]
   in dancers & ix i .~ n & ix j .~ m

permute :: Permutation -> Vector a -> Vector a
permute (Permutation (relist . VF.toList -> p)) v = backpermute v p

dancePerm :: [Dance] -> Permutation
dancePerm = toPerm . foldl' (flip dance) (relist ['a' .. 'p'])

part1 :: String
part1 = relist . permute (dancePerm in16) $ relist ['a' .. 'p']

permTimes :: Int -> [Dance] -> Permutation
permTimes n l = dancePerm (fold $ replicate n l)

order :: Int
order = maybe 0 succ . elemIndex mempty $ [1 ..] <&> \n -> permTimes n in16

part2 :: String
part2 = relist . permute (permTimes (1000000000 `mod` order) in16) $ relist ['a' .. 'p']
