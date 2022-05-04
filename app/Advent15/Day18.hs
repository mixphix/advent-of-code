module Day18 where

import Control.Comonad
import Control.Comonad.Representable.Store
import Control.Lens
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vec

light :: Parser Bool
light = (True <$ char '#') <|> (False <$ char '.')

newtype Vec a = Vec {unV :: Vector a}
  deriving (Eq, Show, Functor, Foldable)

instance Distributive Vec where
  distribute = distributeRep

instance Representable Vec where
  type Rep Vec = Word8
  index (Vec v) i = v Vec.! fromIntegral i
  tabulate d = Vec $ Vec.generate 100 (d . fromIntegral)

type Grid a = Store (Compose Vec Vec) a

toGrid :: Set (Word8, Word8) -> Grid Bool
toGrid s = store (`member` s) (0, 0)

fromGrid :: Grid Bool -> Set (Word8, Word8)
fromGrid (StoreT (Identity (Compose (Vec v))) _) = relist @[] $ do
  (a, Vec v') <- zip [0 ..] (relist v)
  (b, el) <- zip [0 ..] (relist v')
  el ? (a, b)

in18 :: Set (Word8, Word8)
in18 =
  Set.map (\(Point2 x y) -> (fromIntegral x, fromIntegral y))
    . Map.keysSet
    . Map.filter id
    . view grid
    $ parse (many1 light) <$> lines (input 2015 18)

step :: [(Word8, Word8)] -> Grid Bool -> Bool
step alwayson p
  | pos p `elem` alwayson = True
  | extract p = neighbours `elem` [2, 3]
  | otherwise = neighbours == 3
 where
  neighbours =
    count (((`elem` alwayson) . fst) ||^ snd) $ getCompose (experiment moore' p)

moore' :: (Word8, Word8) -> Compose [] ((,) (Word8, Word8)) (Word8, Word8)
moore' =
  Compose
    . map dup
    . filter (all ((>= 0) &&^ (< 100)) . (^.. both))
    . traverseOf (from pair) moore

part1 :: Natural
part1 = maybe 0 (count every . fromGrid) $ iterate' (extend (step [])) (toGrid in18) !!? 100

part2 :: Natural
part2 = maybe 0 (count every . fromGrid) $ iterate' (extend (step ons)) (toGrid in18) !!? 100
 where
  ons = [(0, 0), (0, 99), (99, 0), (99, 99)]
