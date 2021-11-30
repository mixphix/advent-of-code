module Advent.Maps (Mop, IntMop) where

import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.Map.Monoidal.Strict (MonoidalMap)

type Mop = MonoidalMap

type IntMop = MonoidalIntMap
