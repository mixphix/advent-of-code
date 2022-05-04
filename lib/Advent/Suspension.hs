module Advent.Suspension where

import Relude (Bounded (..), Eq, Ord, Show)

data Suspension a
  = SouthPole
  | Meridian a
  | NorthPole
  deriving (Eq, Ord, Show)

instance Bounded (Suspension a) where
  minBound = SouthPole
  maxBound = NorthPole
