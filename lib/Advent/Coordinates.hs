{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Advent.Coordinates where

import Advent.D4 (D4 (..))
import Advent.Functions (relist, (>-<))
import Advent.Orphans ()
import Algebra.Lattice.Levitated
import Control.Lens (Iso', Lens', abbreviatedFields, from, iso, makeLensesWith)
import Data.Complex (Complex (..))
import Data.Foldable.Toolbox (sumOn)
import Data.Ratio ((%))
import Data.Semiring (Ring (..), Semiring (plus, times, zero), minus)
import Data.Semiring qualified as R
import Data.Set.NonEmpty (NESet)
import GHC.Show qualified (show)
import Geometry (Additive ((^-^)), Arity, Point (..))
import Relude hiding (negate)

int :: (Integral n, Ring r) => n -> r
int = R.fromIntegral

complex :: Iso' (Point 2 a) (Complex a)
complex = iso (\(Point2 x y) -> x :+ y) (\(x :+ y) -> Point2 x y)

pair :: Iso' (Point 2 a) (a, a)
pair = iso (\(Point2 x y) -> (x, y)) (uncurry Point2)

cpair :: Iso' (Complex a) (a, a)
cpair = from complex . pair

manhattan :: (Arity d) => Point d Integer -> Point d Integer -> Natural
manhattan (Point v) (Point w) = fromIntegral $ sumOn abs (v ^-^ w)

dihedralTransform :: (Ring a) => D4 -> Point 2 a -> Point 2 a
dihedralTransform = \case
  E -> id
  R -> \(Point2 x y) -> Point2 (negate y) x
  R2 -> fmap negate
  R3 -> \(Point2 x y) -> Point2 y (negate x)
  T -> \(Point2 x y) -> Point2 (negate x) y
  TR -> \(Point2 x y) -> Point2 (negate y) (negate x)
  TR2 -> \(Point2 x y) -> Point2 x (negate y)
  TR3 -> \(Point2 x y) -> Point2 y x

dihedralTransformC :: (Ring a) => D4 -> Complex a -> Complex a
dihedralTransformC = \case
  E -> id
  R -> times (zero :+ R.one)
  R2 -> fmap negate
  R3 -> times (zero :+ negate R.one)
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

cardinal :: (Ring a) => Integer -> Cardinal -> Point 2 a -> Point 2 a
cardinal n c (Point2 x y) = case c of
  East -> Point2 (x `plus` int n) y
  Northeast -> Point2 (x `plus` int n) (y `plus` int n)
  North -> Point2 x (y `plus` int n)
  Northwest -> Point2 (x `minus` int n) (y `plus` int n)
  West -> Point2 (x `minus` int n) y
  Southwest -> Point2 (x `minus` int n) (y `minus` int n)
  South -> Point2 x (y `minus` int n)
  Southeast -> Point2 (x `plus` int n) (y `minus` int n)

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

vonNeumann :: (Ring a) => Point 2 a -> [Point 2 a]
vonNeumann = (cardinal 1 <$> [North, East, South, West] ??)

taxicab :: (Ord a, Ring a) => Natural -> Point 2 a -> NESet (Point 2 a)
taxicab n0 = relist . go n0 . one
 where
  go 0 sp = sp
  go n sp = go (pred n) sp >-< relist @_ @Set . vonNeumann

moore :: (Ring a) => Point 2 a -> [Point 2 a]
moore = (cardinal 1 <$> universe ??)

surrounding :: (Ord a, Ring a) => Natural -> Point 2 a -> NESet (Point 2 a)
surrounding n0 = relist . go n0 . one
 where
  go 0 sp = sp
  go n sp = go (pred n) sp >-< relist @_ @Set . moore

data Ant = Ant
  { antDirection :: Cardinal
  , antPosition :: Point 2 Integer
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
antCentre c = Ant c (Point2 0 0)

slope :: Point 2 Integer -> Point 2 Integer -> Levitated Rational
slope (Point2 x0 y0) (Point2 x1 y1)
  | x0 == x1 && y1 > y0 = Top
  | x0 == x1 = Bottom
  | otherwise = Levitate $ (y1 - y0) % (x1 - x0)
