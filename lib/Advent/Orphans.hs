{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Advent.Orphans
  ( NEVector,
    CVector,
    V,
    NEMop (..),
    NEIntMop (..),
    pattern Empty,
    pattern NonEmpty,
  )
where

import Advent.Maps
import Control.Lens hiding (pattern Empty)
import Data.Containers.NonEmpty (HasNonEmpty (..), pattern IsEmpty, pattern IsNonEmpty)
import Data.Functor.Classes (Eq1, Ord1, Show1)
import Data.Geometry.Point (Point)
import Data.Geometry.Vector (Arity)
import Data.Geometry.Vector qualified
import Data.IntMap.Monoidal.Strict qualified as IntMop
import Data.IntMap.NonEmpty (NEIntMap)
import Data.IntMap.NonEmpty qualified as NEIntMap
import Data.IntSet.NonEmpty (NEIntSet)
import Data.IntSet.NonEmpty qualified as NEIntSet
import Data.Map.Deep (DeepMap)
import Data.Map.Deep qualified as DM
import Data.Map.Monoidal.Strict qualified as Mop
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe (fromJust)
import Data.Monoid.Toolbox (Max (..), Min (..))
import Data.Semigroup.Foldable qualified
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Circular (CircularVector (..))
import Data.Vector.Circular qualified as CVector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import GHC.Exts qualified
import GHC.TypeNats (type (<=))
import Relude.Extra (DynamicMap (..), Foldable1 (..), StaticMap (..))

instance Rewrapped (Mop k a) (Mop k a)

instance Rewrapped (IntMop a) (IntMop a)

type NEVector = NonEmptyVector

type CVector = CircularVector

newtype NEMop k a = NEMop {getNEMap :: NEMap k a}
  deriving (Eq, Ord, Show)
  deriving newtype
    ( Eq1,
      Ord1,
      Show1,
      Functor,
      Foldable,
      FunctorWithIndex k,
      FoldableWithIndex k,
      Data.Semigroup.Foldable.Foldable1,
      Foldable1
    )

instance Traversable (NEMop k) where
  traverse f (NEMop m) = NEMop <$> traverse f m

instance TraversableWithIndex k (NEMop k) where
  itraverse f (NEMop m) = NEMop <$> itraverse f m

instance Wrapped (NEMop k a) where
  type Unwrapped _ = NEMap k a
  _Wrapped' = iso getNEMap NEMop

instance Rewrapped (NEMop k a) (NEMop k a)

instance Rewrapped (NEMop k a) (NEMap k a)

newtype NEIntMop a = NEIntMop {getNEIntMap :: NEIntMap a}
  deriving (Eq, Ord, Show)
  deriving newtype
    ( Eq1,
      Ord1,
      Show1,
      Functor,
      Foldable,
      FunctorWithIndex Int,
      FoldableWithIndex Int,
      Data.Semigroup.Foldable.Foldable1,
      Foldable1
    )

instance Wrapped (NEIntMop a) where
  type Unwrapped _ = NEIntMap a
  _Wrapped' = iso getNEIntMap NEIntMop

instance Rewrapped (NEIntMop a) (NEIntMop a)

instance Rewrapped (NEIntMop a) (NEIntMap a)

instance HasNonEmpty (Mop k a) where
  type NE _ = NEMop k a
  withNonEmpty x y (Mop.MonoidalMap m) = withNonEmpty x (y . NEMop) m
  empty = Empty
  fromNonEmpty = Wrapped . fromNonEmpty . Unwrapped

instance HasNonEmpty (IntMop a) where
  type NE _ = NEIntMop a
  withNonEmpty x y (IntMop.MonoidalIntMap m) = withNonEmpty x (y . NEIntMop) m
  empty = Empty
  fromNonEmpty = Wrapped . fromNonEmpty . Unwrapped

instance Traversable NEIntMop where
  traverse f (NEIntMop m) = NEIntMop <$> traverse f m

instance TraversableWithIndex Int NEIntMop where
  itraverse f (NEIntMop m) = NEIntMop <$> itraverse f m

type V = Data.Geometry.Vector.Vector

-- Folds and listlikes
instance One (Vector a) where
  type OneItem _ = a
  one = pure

instance One (NEVector a) where
  type OneItem _ = a
  one = pure

instance IsList (NEVector a) where
  type Item _ = a
  fromList = NEVector.unsafeFromVector . Vector.fromList
  toList = NEVector.toList

instance Data.Semigroup.Foldable.Foldable1 NEVector where
  foldMap1 f (NEVector.toVector -> v :: Vector a) = fromJust $ foldMap (Just . f) v

instance Foldable1 NEVector where
  foldMap1 = Data.Semigroup.Foldable.foldMap1

instance One (CVector a) where
  type OneItem _ = a
  one a = CircularVector (one a) 0

instance IsList (CVector a) where
  type Item _ = a
  fromList = CVector.unsafeFromVector . Vector.fromList
  toList = CVector.toList

instance Foldable1 CVector where
  foldMap1 = Data.Semigroup.Foldable.foldMap1

instance (Arity d, 1 <= d) => Foldable1 (Point d) where
  foldMap1 f v = fromJust $ foldMap (Just . f) v

instance (Arity d, 1 <= d) => Foldable1 (V d) where
  foldMap1 f v = fromJust $ foldMap (Just . f) v

instance One (NESeq a) where
  type OneItem _ = a
  one = pure

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

instance One NEIntSet where
  type OneItem _ = Int
  one = fromList . one

instance IsList NEIntSet where
  type Item _ = Int
  fromList xs = viaNonEmpty NEIntSet.fromList xs ?: error "NEIntSet: fromList []"
  toList = toList . NEIntSet.toList

instance (Ord k) => One (NEMap k a) where
  type OneItem _ = (k, a)
  one = fromList . one

instance (Ord k) => IsList (NEMap k a) where
  type Item _ = (k, a)
  fromList xs = viaNonEmpty NEMap.fromList xs ?: error "NEMap: fromList []"
  toList = toList . NEMap.assocs

instance Foldable1 (NEMap k) where
  foldMap1 = Data.Semigroup.Foldable.foldMap1

instance (Ord k) => StaticMap (NEMap k v) where
  type Key _ = k
  type Val _ = v
  size = NEMap.size
  lookup = NEMap.lookup
  member = NEMap.member

instance (Ord k) => DynamicMap (NEMap k v) where
  insert = NEMap.insert
  insertWith = NEMap.insertWith
  delete k = NEMap.unsafeFromMap . NEMap.delete k
  alter f k = NEMap.unsafeFromMap . NEMap.alter f k

instance One (NEIntMap a) where
  type OneItem _ = (Int, a)
  one = fromList . one

instance IsList (NEIntMap a) where
  type Item _ = (Int, a)
  fromList xs = viaNonEmpty NEIntMap.fromList xs ?: error "NEIntMap: fromList []"
  toList = toList . NEIntMap.assocs

instance Foldable1 NEIntMap where
  foldMap1 = Data.Semigroup.Foldable.foldMap1

instance StaticMap (NEIntMap v) where
  type Key _ = Int
  type Val _ = v
  size = NEIntMap.size
  lookup = NEIntMap.lookup
  member = NEIntMap.member

instance DynamicMap (NEIntMap v) where
  insert = NEIntMap.insert
  insertWith = NEIntMap.insertWith
  delete k = NEIntMap.unsafeFromMap . NEIntMap.delete k
  alter f k = NEIntMap.unsafeFromMap . NEIntMap.alter f k

instance (Semigroup v) => One (IntMop v) where
  type OneItem _ = (Int, v)
  one = IntMop.fromList . one

instance StaticMap (IntMop v) where
  type Key _ = Int
  type Val _ = v
  size = IntMop.size
  lookup = IntMop.lookup
  member = IntMop.member

instance (Semigroup v) => DynamicMap (IntMop v) where
  insert = IntMop.insert
  insertWith = IntMop.insertWith
  delete = IntMop.delete
  alter = IntMop.alter

instance One (NEIntMop a) where
  type OneItem _ = (Int, a)
  one = fromList . one

instance IsList (NEIntMop a) where
  type Item _ = (Int, a)
  fromList xs = Wrapped $ viaNonEmpty NEIntMap.fromList xs ?: error "NEIntMap: fromList []"
  toList = toList . NEIntMap.assocs . Unwrapped

instance StaticMap (NEIntMop v) where
  type Key _ = Int
  type Val _ = Val (IntMop v)
  size = NEIntMap.size . Unwrapped
  lookup k = NEIntMap.lookup k . Unwrapped
  member k = NEIntMap.member k . Unwrapped

instance DynamicMap (NEIntMop v) where
  insert = coerce <<$>> NEIntMap.insert
  insertWith = (coerce <<$>>) . NEIntMap.insertWith
  delete k = Wrapped . NEIntMap.unsafeFromMap . NEIntMap.delete k . Unwrapped
  alter f k = Wrapped . NEIntMap.unsafeFromMap . NEIntMap.alter f k . Unwrapped

instance (Ord k, Semigroup v) => One (Mop k v) where
  type OneItem _ = (k, v)
  one = Mop.fromList . one

instance (Ord k) => StaticMap (Mop k v) where
  type Key _ = k
  type Val _ = v
  size = Mop.size
  lookup = Mop.lookup
  member = Mop.member

instance (Ord k, Semigroup v) => DynamicMap (Mop k v) where
  insert = Mop.insert
  insertWith = Mop.insertWith
  delete = Mop.delete
  alter = Mop.alter

instance (Ord k) => One (NEMop k a) where
  type OneItem _ = (k, a)
  one = fromList . one

instance (Ord k) => IsList (NEMop k a) where
  type Item _ = (k, a)
  fromList xs = Wrapped $ viaNonEmpty NEMap.fromList xs ?: error "NEMap: fromList []"
  toList = toList . NEMap.assocs . Unwrapped

instance (Ord k) => StaticMap (NEMop k v) where
  type Key _ = k
  type Val _ = v
  size = NEMap.size . Unwrapped
  lookup k = NEMap.lookup k . Unwrapped
  member k = NEMap.member k . Unwrapped

instance (Ord k) => DynamicMap (NEMop k v) where
  insert = coerce <<$>> NEMap.insert
  insertWith = (coerce <<$>>) . NEMap.insertWith
  delete k = Wrapped . NEMap.unsafeFromMap . NEMap.delete k . Unwrapped
  alter f k = Wrapped . NEMap.unsafeFromMap . NEMap.alter f k . Unwrapped

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

instance (Ord k, Eq (DeepMap ks v), Semigroup (DeepMap ks v)) => AsEmpty (DeepMap (k ': ks) v)

instance HasNonEmpty (Maybe a) where
  type NE _ = Identity a
  withNonEmpty x y = maybe x (y . Identity)
  empty = Nothing
  fromNonEmpty = Just . runIdentity

pattern Empty :: (HasNonEmpty s, AsEmpty s) => s
pattern Empty = IsEmpty

pattern NonEmpty :: (HasNonEmpty s, AsEmpty s) => NE s -> s
pattern NonEmpty a = IsNonEmpty a

{-# COMPLETE Empty, NonEmpty :: Maybe #-}

{-# COMPLETE Empty, NonEmpty :: [] #-}

{-# COMPLETE Empty, NonEmpty :: Vector #-}

{-# COMPLETE Empty, NonEmpty :: Seq #-}

{-# COMPLETE Empty, NonEmpty :: Set #-}

{-# COMPLETE Empty, NonEmpty :: IntSet #-}

{-# COMPLETE Empty, NonEmpty :: Map #-}

{-# COMPLETE Empty, NonEmpty :: IntMap #-}

{-# COMPLETE Empty, NonEmpty :: Mop #-}

{-# COMPLETE Empty, NonEmpty :: IntMop #-}

{-# COMPLETE Empty, NonEmpty :: DeepMap #-}

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

-- Lenses
type instance Index (NEVector a) = Int

type instance IxValue (NEVector a) = a

instance Ixed (NEVector a) where
  ix i f v
    | 0 <= i && i < length v = f (v NEVector.! i) <&> \a -> v NEVector.// [(i, a)]
    | otherwise = pure v
  {-# INLINE ix #-}

instance Each (NEVector a) (NEVector b) a b

instance FunctorWithIndex Int NEVector

instance FoldableWithIndex Int NEVector

instance TraversableWithIndex Int NEVector

type instance Index (CVector a) = Int

type instance IxValue (CVector a) = a

instance Ixed (CVector a) where
  ix i f cv@(CircularVector v k)
    | 0 <= i && i < length v = f (v NEVector.! i) <&> \a -> CircularVector (v NEVector.// [(i, a)]) k
    | otherwise = pure cv
  {-# INLINE ix #-}

instance FunctorWithIndex Int CVector

instance FoldableWithIndex Int CVector

instance TraversableWithIndex Int CVector

type instance Index (NESeq a) = Int

type instance IxValue (NESeq a) = a

instance Ixed (NESeq a) where
  ix i f s
    | 0 <= i && i < length s = f (NESeq.index s i) <&> \a -> NESeq.adjust' (const a) i s
    | otherwise = pure s
  {-# INLINE ix #-}

instance Each (NESeq a) (NESeq b) a b

instance FunctorWithIndex Int NESeq

instance FoldableWithIndex Int NESeq

instance TraversableWithIndex Int NESeq

type instance Index (NESet a) = a

type instance IxValue (NESet a) = ()

instance (Ord a) => Ixed (NESet a) where
  ix k f m =
    if NESet.member k m
      then f () $> m
      else pure m
  {-# INLINE ix #-}

type instance Index NEIntSet = Int

type instance IxValue NEIntSet = ()

instance Ixed NEIntSet where
  ix k f m =
    if NEIntSet.member k m
      then f () $> m
      else pure m
  {-# INLINE ix #-}

type instance Index (NEMap k v) = k

type instance IxValue (NEMap k v) = v

instance (Ord k) => Ixed (NEMap k v) where
  ix k f m = case lookup k m of
    Just v -> f v <&> \v' -> insert k v' m
    Nothing -> pure m
  {-# INLINE ix #-}

instance FunctorWithIndex k (NEMap k)

instance FoldableWithIndex k (NEMap k)

instance TraversableWithIndex k (NEMap k) where
  itraverse = NEMap.traverseWithKey

type instance Index (NEIntMap v) = Int

type instance IxValue (NEIntMap v) = v

instance Ixed (NEIntMap v) where
  ix k f m = case lookup k m of
    Just v -> f v <&> \v' -> insert k v' m
    Nothing -> pure m
  {-# INLINE ix #-}

instance Each (NEIntMap a) (NEIntMap b) a b

instance FunctorWithIndex Int NEIntMap

instance FoldableWithIndex Int NEIntMap

instance TraversableWithIndex Int NEIntMap

type instance Index (DeepMap (k ': ks) v) = k

type instance IxValue (DeepMap (k ': ks) v) = DeepMap ks v

instance (Ord k, Semigroup (DeepMap ks v)) => Ixed (DeepMap (k ': ks) v) where
  ix k f m = case lookup k m of
    Just v -> f v <&> \v' -> insert k v' m
    Nothing -> pure m
  {-# INLINE ix #-}

instance (Ord k, Semigroup (DeepMap ks v)) => At (DeepMap (k ': ks) v) where
  at k f = DM.alterF f k
  {-# INLINE at #-}
