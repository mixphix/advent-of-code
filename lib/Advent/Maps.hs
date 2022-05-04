{-# LANGUAGE FlexibleContexts #-}

module Advent.Maps (Mop, (!), IntMop) where

import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Relude (Monoid (mempty), Ord, (?:))
import Relude.Extra.Map (DynamicMap, StaticMap (Key, Val), (!?))

type Mop = MonoidalMap

type IntMop = MonoidalIntMap

(!) :: (Ord (Key m), Monoid (Val m), DynamicMap m) => m -> Key m -> Val m
(!) m k = m !? k ?: mempty
