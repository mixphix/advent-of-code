{-# LANGUAGE FlexibleContexts #-}

module Advent.Maps (Mop, (!), IntMop) where

import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Relude.Extra.Map

type Mop = MonoidalMap

type IntMop = MonoidalIntMap

(!) :: (Ord (Key m), Monoid (Val m), DynamicMap m) => m -> Key m -> Val m
(!) m k = m !? k ?: mempty
