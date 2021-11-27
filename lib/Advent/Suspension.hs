module Advent.Suspension where

data Suspension a
  = SouthPole
  | Meridian a
  | NorthPole
  deriving (Eq, Ord, Show)

instance Bounded (Suspension a) where
  minBound = SouthPole
  maxBound = NorthPole
