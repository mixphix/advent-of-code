{-# LANGUAGE FlexibleContexts #-}

module Advent.Maps (one, Mop, (!), IntMop, (@=)) where

import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Relude (Monoid (mempty), One (OneItem), Ord, curry, (?:))
import Relude.Container.One qualified as One (one)
import Relude.Extra.Map (DynamicMap, StaticMap (Key, Val), (!?))

one :: forall f x. One (f x) => OneItem (f x) -> f x
one = One.one

type Mop = MonoidalMap

type IntMop = MonoidalIntMap

(!) :: (Ord (Key m), Monoid (Val m), DynamicMap m) => m -> Key m -> Val m
(!) m k = m !? k ?: mempty

(@=) :: forall f x y. (One (f x y), OneItem (f x y) ~ (x, y)) => x -> y -> f x y
(@=) = curry one
