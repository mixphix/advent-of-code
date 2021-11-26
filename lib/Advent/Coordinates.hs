{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Advent.Coordinates where

import Advent.D4 (D4 (..))
import Control.Lens (Iso', Lens', abbreviatedFields, from, iso, makeLensesWith)
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

cpair :: Iso' (Complex a) (a, a)
cpair = from complex . pair

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

dihedralGrid :: D4 -> [[a]] -> [[a]]
dihedralGrid = \case
  E -> id
  R -> reverse . transpose
  R2 -> reverse . map reverse
  R3 -> transpose . reverse
  T -> map reverse
  TR -> transpose
  TR2 -> reverse
  TR3 -> reverse . transpose . reverse

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

data Ant = Ant
  { antDirection :: Cardinal,
    antPosition :: V2 Integer
  }
  deriving (Eq)

makeLensesWith abbreviatedFields ''Ant

instance Show Ant where
  show (Ant c v) =
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
turnL (Ant c p) = Ant c' p
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
turnR (Ant c p) = Ant c' p
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

turnU :: Ant -> Ant
turnU = turnR . turnR

scurry :: Integer -> Ant -> Ant
scurry 0 a = a
scurry n (Ant c p) = Ant c (cardinal n c p)

shimmy :: Integer -> Cardinal -> Ant -> Ant
shimmy n c (Ant ac p) = Ant ac (cardinal n c p)

positionC :: Lens' Ant (Complex Integer)
positionC = position . complex

antCentre :: Cardinal -> Ant
antCentre c = Ant c (V2 0 0)
