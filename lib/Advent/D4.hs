{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Advent.D4 where

import Data.Group (Group (..))
import Relude

data D4
  = E
  | R
  | R2
  | R3
  | T
  | TR
  | TR2
  | TR3
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Semigroup D4 where
  x <> E = x
  E <> x = x
  R <> x = case x of
    R -> R2
    R2 -> R3
    R3 -> E
    T -> TR3
    TR -> T
    TR2 -> TR
    TR3 -> TR2
  R2 <> x = case x of
    R -> R3
    R2 -> E
    R3 -> R
    T -> TR2
    TR -> TR
    TR2 -> T
    TR3 -> TR3
  R3 <> x = case x of
    R -> E
    R2 -> R
    R3 -> R2
    T -> TR
    TR -> TR3
    TR2 -> TR
    TR3 -> TR2
  T <> x = case x of
    R -> TR
    R2 -> TR2
    R3 -> TR2
    T -> E
    TR -> R
    TR2 -> R2
    TR3 -> R3
  TR <> x = case x of
    R -> TR2
    R2 -> TR3
    R3 -> T
    T -> R3
    TR -> E
    TR2 -> R
    TR3 -> R2
  TR2 <> x = case x of
    R -> TR3
    R2 -> T
    R3 -> TR
    T -> R2
    TR -> R3
    TR2 -> E
    TR3 -> R
  TR3 <> x = case x of
    R -> T
    R2 -> TR
    R3 -> TR2
    T -> R
    TR -> R2
    TR2 -> R3
    TR3 -> E

instance Monoid D4 where mempty = E

instance Group D4 where
  invert = \case
    E -> E
    R -> R3
    R2 -> R2
    R3 -> R
    T -> T
    TR -> TR
    TR2 -> TR2
    TR3 -> TR3
