module Day18 where

import Advent
import Control.Comonad
import Control.Comonad.Representable.Store
import Control.Lens
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.List.Toolbox (iterate')
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as V

light :: Parser Bool
light = (True <$ char '#') <|> (False <$ char '.')

newtype V a = V {unV :: Vector a}
  deriving (Eq, Show, Functor, Foldable)

instance Distributive V where
  distribute = distributeRep

instance Representable V where
  type Rep V = Word8
  index (V v) i = v V.! fromIntegral i
  tabulate d = V $ V.generate 100 (d . fromIntegral)

type Grid a = Store (Compose V V) a

toGrid :: Set (Word8, Word8) -> Grid Bool
toGrid s = store (`member` s) (0, 0)

fromGrid :: Grid Bool -> Set (Word8, Word8)
fromGrid (StoreT (Identity (Compose (V v))) _) = relist @[] $ do
  (a, V v') <- zip [0 ..] (relist v)
  (b, el) <- zip [0 ..] (relist v')
  el ? (a, b)

in18 :: Set (Word8, Word8)
in18 =
  Set.map (\(V2 x y) -> (fromIntegral x, fromIntegral y))
    . Map.keysSet
    . Map.filter id
    . view grid
    . mapMaybe (parsedWith $ many1 light)
    $ lines (input 2015 18)

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
