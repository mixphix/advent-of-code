{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Advent.Orphans where

import Data.IntMap.Monoidal.Strict qualified as IntMop
import Data.IntMap.NonEmpty (NEIntMap)
import Data.IntMap.NonEmpty qualified as NEIntMap
import Data.Map.Monoidal.Deep (DeepMap)
import Data.Map.Monoidal.Deep qualified as DM
import Data.Map.Monoidal.Strict qualified as Mop
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Monoid.Toolbox (Max (..), Min (..))
import Data.Semigroup.Foldable qualified
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import GHC.Exts qualified
import Relude.Extra

instance One (NESeq a) where
  type OneItem _ = a
  one = fromList . one

instance IsList (NESeq a) where
  type Item _ = a
  fromList xs = viaNonEmpty NESeq.fromList xs ?: error "NESeq: fromList []"
  toList = toList

instance Foldable1 NESeq where
  foldMap1 = Data.Semigroup.Foldable.foldMap1

instance (Ord a) => One (NESet a) where
  type OneItem _ = a
  one = fromList . one

instance (Ord a) => IsList (NESet a) where
  type Item _ = a
  fromList xs = viaNonEmpty NESet.fromList xs ?: error "NESet: fromList []"
  toList = toList

instance Foldable1 NESet where
  foldMap1 = Data.Semigroup.Foldable.foldMap1

instance One (NEIntMap a) where
  type OneItem _ = (Int, a)
  one = fromList . one

instance IsList (NEIntMap a) where
  type Item _ = (Int, a)
  fromList xs = viaNonEmpty NEIntMap.fromList xs ?: error "NEIntMap: fromList []"
  toList = toList . NEIntMap.assocs

instance Foldable1 NEIntMap where
  foldMap1 = Data.Semigroup.Foldable.foldMap1

instance (Ord k) => One (NEMap k a) where
  type OneItem _ = (k, a)
  one = fromList . one

instance (Ord k) => IsList (NEMap k a) where
  type Item _ = (k, a)
  fromList xs = viaNonEmpty NEMap.fromList xs ?: error "NEMap: fromList []"
  toList = toList . NEMap.assocs

instance Foldable1 (NEMap k) where
  foldMap1 = Data.Semigroup.Foldable.foldMap1

instance StaticMap (NEIntMap v) where
  type Key _ = Int
  type Val _ = v
  size = NEIntMap.size
  lookup = NEIntMap.lookup
  member = NEIntMap.member

instance (Semigroup v) => DynamicMap (NEIntMap v) where
  insert = NEIntMap.insert
  insertWith = NEIntMap.insertWith
  delete k = NEIntMap.unsafeFromMap . NEIntMap.delete k
  alter f k = NEIntMap.unsafeFromMap . NEIntMap.alter f k

instance (Semigroup v) => One (IntMop.IntMap v) where
  type OneItem _ = (Int, v)
  one = IntMop.fromList . one

instance StaticMap (IntMop.IntMap v) where
  type Key _ = Int
  type Val _ = v
  size = IntMop.size
  lookup = IntMop.lookup
  member = IntMop.member

instance (Semigroup v) => DynamicMap (IntMop.IntMap v) where
  insert = IntMop.insert
  insertWith = IntMop.insertWith
  delete = IntMop.delete
  alter = IntMop.alter

instance (Ord k, Semigroup v) => One (Mop.Map k v) where
  type OneItem _ = (k, v)
  one = Mop.fromList . one

instance (Ord k) => StaticMap (Mop.Map k v) where
  type Key _ = k
  type Val _ = v
  size = Mop.size
  lookup = Mop.lookup
  member = Mop.member

instance (Ord k, Semigroup v) => DynamicMap (Mop.Map k v) where
  insert = Mop.insert
  insertWith = Mop.insertWith
  delete = Mop.delete
  alter = Mop.alter

instance (Ord k, Semigroup (DeepMap ks v)) => One (DeepMap (k ': ks) v) where
  type OneItem _ = (k, DeepMap ks v)
  one = DM.fromList . one

instance (Ord k) => StaticMap (DeepMap (k ': ks) v) where
  type Key _ = k
  type Val _ = DeepMap ks v
  size = DM.size
  lookup = DM.lookup
  member = DM.member

instance (Ord k, Semigroup (DeepMap ks v)) => DynamicMap (DeepMap (k ': ks) v) where
  insert = DM.insert
  insertWith = DM.insertWith
  delete = DM.delete
  alter = DM.alter

-- Sum instances
deriving newtype instance (Enum a) => Enum (Sum a)

deriving newtype instance (Real a) => Real (Sum a)

deriving newtype instance (Fractional a) => Fractional (Sum a)

deriving newtype instance (Floating a) => Floating (Sum a)

deriving newtype instance (RealFrac a) => RealFrac (Sum a)

deriving newtype instance (RealFloat a) => RealFloat (Sum a)

deriving newtype instance (Integral a) => Integral (Sum a)

-- Product instances
deriving newtype instance (Enum a) => Enum (Product a)

deriving newtype instance (Real a) => Real (Product a)

deriving newtype instance (Fractional a) => Fractional (Product a)

deriving newtype instance (Floating a) => Floating (Product a)

deriving newtype instance (RealFrac a) => RealFrac (Product a)

deriving newtype instance (RealFloat a) => RealFloat (Product a)

deriving newtype instance (Integral a) => Integral (Product a)

-- Max instances
deriving newtype instance (Real a) => Real (Max a)

deriving newtype instance (Fractional a) => Fractional (Max a)

deriving newtype instance (Floating a) => Floating (Max a)

deriving newtype instance (RealFrac a) => RealFrac (Max a)

deriving newtype instance (RealFloat a) => RealFloat (Max a)

deriving newtype instance (Integral a) => Integral (Max a)

-- Min instances
deriving newtype instance (Real a) => Real (Min a)

deriving newtype instance (Fractional a) => Fractional (Min a)

deriving newtype instance (Floating a) => Floating (Min a)

deriving newtype instance (RealFrac a) => RealFrac (Min a)

deriving newtype instance (RealFloat a) => RealFloat (Min a)

deriving newtype instance (Integral a) => Integral (Min a)
