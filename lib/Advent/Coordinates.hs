module Advent.Coordinates where

import Advent.D4 (D4 (..))
import Control.Lens (Iso', from, iso, view, (^.))
import Data.Complex (Complex (..))
import Data.Semiring (Ring (..), Semiring (..), fromIntegral, minus)
import GHC.Show qualified (show)
import Linear (V2 (..))
import Prelude hiding (negate, one)

int :: (Integral n, Ring r) => n -> r
int = Data.Semiring.fromIntegral

complex :: Iso' (V2 a) (Complex a)
complex = iso (\(V2 x y) -> x :+ y) (\(x :+ y) -> V2 x y)

pair :: Iso' (V2 a) (a, a)
pair = iso (\(V2 x y) -> (x, y)) (uncurry V2)

dihedralTransform :: (Ring a) => D4 -> V2 a -> V2 a
dihedralTransform = \case
  E -> id
  R -> \(V2 x y) -> V2 (negate y) x
  R2 -> fmap negate
  R3 -> \(V2 x y) -> V2 y (negate x)
  T -> \(V2 x y) -> V2 (negate x) y
  TR -> \(V2 x y) -> V2 (negate y) (negate x)
  TR2 -> \(V2 x y) -> V2 x (negate y)
  TR3 -> \(V2 x y) -> V2 y x

dihedralTransformC :: (Ring a) => D4 -> Complex a -> Complex a
dihedralTransformC = \case
  E -> id
  R -> times (zero :+ one)
  R2 -> fmap negate
  R3 -> times (zero :+ negate one)
  T -> \(x :+ y) -> negate x :+ y
  TR -> \(x :+ y) -> negate y :+ negate x
  TR2 -> \(x :+ y) -> x :+ negate y
  TR3 -> \(x :+ y) -> y :+ x

(×) :: D4 -> V2 Integer -> V2 Integer
(×) = dihedralTransform

(•) :: D4 -> Complex Integer -> Complex Integer
(•) = dihedralTransformC

data Cardinal
  = East
  | Northeast
  | North
  | Northwest
  | West
  | Southwest
  | South
  | Southeast
  deriving (Eq, Ord, Enum, Bounded, Show)

cardinal :: (Ring a) => Integer -> Cardinal -> V2 a -> V2 a
cardinal n c (V2 x y) = case c of
  East -> V2 (x `plus` int n) y
  Northeast -> V2 (x `plus` int n) (y `plus` int n)
  North -> V2 x (y `plus` int n)
  Northwest -> V2 (x `minus` int n) (y `plus` int n)
  West -> V2 (x `minus` int n) y
  Southwest -> V2 (x `minus` int n) (y `minus` int n)
  South -> V2 x (y `minus` int n)
  Southeast -> V2 (x `plus` int n) (y `minus` int n)

cardinalC :: (Ring a) => Integer -> Cardinal -> Complex a -> Complex a
cardinalC n c (x :+ y) = case c of
  East -> (x `plus` int n) :+ y
  Northeast -> (x `plus` int n) :+ (y `plus` int n)
  North -> x :+ (y `plus` int n)
  Northwest -> (x `minus` int n) :+ (y `plus` int n)
  West -> (x `minus` int n) :+ y
  Southwest -> (x `minus` int n) :+ (y `minus` int n)
  South -> x :+ (y `minus` int n)
  Southeast -> (x `plus` int n) :+ (y `minus` int n)

moore :: (Ring a) => V2 a -> [V2 a]
moore = (cardinal 1 <$> universe ??)

vonNeumann :: (Ring a) => V2 a -> [V2 a]
vonNeumann = (cardinal 1 <$> [North, East, South, West] ??)

newtype Ant = Ant (Cardinal, Complex Integer) deriving (Eq)

instance Show Ant where
  show (Ant (c, view (from complex) -> v)) =
    "Ant (" <> show v <> ") " <> case c of
      East -> "\x27a1"
      Northeast -> "\x2197"
      North -> "\x2b06"
      Northwest -> "\x2196"
      West -> "\x2b05"
      Southwest -> "\x2199"
      South -> "\x2b07"
      Southeast -> "\x2198"

turnL :: Ant -> Ant
turnL (Ant (c, p)) = Ant (c', p)
  where
    c' = case c of
      East -> North
      Northeast -> Northwest
      North -> West
      Northwest -> Southwest
      West -> South
      Southwest -> Southeast
      South -> East
      Southeast -> Northeast

turnR :: Ant -> Ant
turnR (Ant (c, p)) = Ant (c', p)
  where
    c' = case c of
      East -> South
      Northeast -> Southeast
      North -> East
      Northwest -> Northeast
      West -> North
      Southwest -> Northwest
      South -> West
      Southeast -> Southwest

scurry :: Natural -> Ant -> Ant
scurry 0 a = a
scurry n (Ant (c, p)) = Ant (c, cardinalC (Prelude.fromIntegral n) c p)

shimmy :: Natural -> Cardinal -> Ant -> Ant
shimmy n c (Ant (ac, p)) = Ant (ac, cardinalC (Prelude.fromIntegral n) c p)

antPos :: Ant -> Complex Integer
antPos (Ant (_, p)) = p

antPosV :: Ant -> V2 Integer
antPosV a = antPos a ^. from complex

antCentre :: Cardinal -> Ant
antCentre c = Ant (c, 0 :+ 0)
