module Advent (module Advent) where

import Advent.Coordinates as Advent
import Advent.D4 as Advent
import Advent.Functions as Advent
import Advent.Input as Advent
import Advent.Maps as Advent
import Advent.Numbers as Advent
import Advent.Orphans as Advent
import Advent.Parsers as Advent
import Advent.Polynomials as Advent
import Advent.Suspension as Advent
import AdventAPI as Advent (Part (..))
import Control.Applicative as Advent
import Control.Arrow as Advent hiding (ArrowLoop (..), first, second)
import Control.Lens as Advent (at, from, ix, none, to, view, (%~), (.~), (<>~), (?~), (^.), _1, _2, _3, _4)
import Control.Monad as Advent
import Data.Bits as Advent
import Data.Char as Advent
import Data.Complex as Advent hiding (conjugate)
import Data.Containers.ListUtils as Advent
import Data.Containers.NonEmpty as Advent hiding (empty, pattern IsEmpty, pattern IsNonEmpty)
import Data.Data as Advent
import Data.Either.Toolbox as Advent (keepLeft, keepRight)
import Data.Foldable.Toolbox as Advent (firstJust, firstJustM, notNull)
import Data.Function.Toolbox as Advent hiding (applyN, (.:))
import Data.IntMap.NonEmpty as Advent (NEIntMap)
import Data.IntSet.NonEmpty as Advent (NEIntSet)
import Data.List.NonEmpty.Toolbox as Advent (
  group1,
  groupAll,
  groupAllWith,
  groupAllWith1,
  groupBy1,
  groupWith,
  groupWith1,
  (<|),
  (<||),
  (|:),
  (|>),
  (||>),
 )
import Data.List.Toolbox as Advent (
  allSame,
  anySame,
  anySameOrd,
  breakEnd,
  chopInfix,
  chunksOf,
  deleteBy,
  deleteFirstsBy,
  disjoint,
  disjointOrd,
  dropEnd,
  dropWhileEnd,
  dropWhileUnique,
  dropWhileUniqueOrd,
  elemIndex,
  elemIndices,
  enumerate,
  findIndex,
  findIndices,
  genericChunksOf,
  genericCount,
  genericDropEnd,
  genericIndex,
  genericTakeEnd,
  groupOn,
  groupSort,
  groupSortOn,
  insertBy,
  intersect,
  intersectBy,
  intersectOn,
  isInfixOf,
  isSubsequenceOf,
  isSuffixOf,
  iterate',
  iterateWhileUnique,
  list,
  maximumBy,
  merge,
  mergeBy,
  mergeOn,
  minimumBy,
  nub,
  nubBy,
  partition,
  removed,
  replace,
  replaceFirst,
  safeHead,
  safeInit,
  safeLast,
  safeTail,
  spanEnd,
  splitOn,
  stripPrefix,
  sublists,
  takeEnd,
  takeWhileEnd,
  takeWhileUnique,
  takeWhileUniqueOrd,
  tuple2,
  tuple3,
  tuple4,
  union,
  unionBy,
  unionOn,
  unsnoc,
  unzip4,
  unzip5,
  unzip6,
  unzip7,
  zip4,
  zip5,
  zip6,
  zip7,
  zipWith3,
  zipWith4,
  zipWith5,
  zipWith6,
  zipWith7,
  (\\),
 )
import Data.Map.NonEmpty as Advent (NEMap)
import Data.Monoid.Toolbox as Advent hiding (option)
import Data.Sequence.NonEmpty as Advent (NESeq)
import Data.Set.NonEmpty as Advent (NESet)
import Data.Text as Advent (strip)
import Data.Traversable as Advent
import Data.Vector as Advent (Vector)
import Geometry.Point as Advent
import Geometry.Vector as Advent hiding (Vector, head, init, last, replicate)
import Relude as Advent hiding (First (..), Last (..), map, nonEmpty, (&&^), (||^))
import Relude.Extra.Enum as Advent
import Relude.Extra.Foldable1 as Advent
import Relude.Extra.Group as Advent
import Relude.Extra.Map as Advent
import Relude.Extra.Newtype as Advent
import Relude.Extra.Tuple as Advent
import Text.Parsec as Advent hiding (State (..), choice, count, many, optional, parse, uncons, (<|>), pattern Empty)
import Text.Parsec.Text as Advent (Parser)

applyN :: Natural -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN n f a = f $! applyN (pred n) f a

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap
