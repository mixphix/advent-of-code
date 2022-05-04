module Prelude (module Prelude) where

import Advent.Coordinates as Prelude
import Advent.D4 as Prelude
import Advent.Functions as Prelude
import Advent.Input as Prelude
import Advent.Maps as Prelude
import Advent.Numbers as Prelude
import Advent.Orphans as Prelude
import Advent.Parsers as Prelude
import Advent.Polynomials as Prelude
import Advent.Suspension as Prelude
import AdventAPI as Prelude (Part (..))
import Control.Applicative as Prelude
import Control.Arrow as Prelude hiding (ArrowLoop (..), first, second)
import Control.Lens as Prelude (at, from, ix, none, to, view, (%~), (.~), (<>~), (?~), (^.), _1, _2, _3, _4)
import Control.Monad as Prelude
import Data.Bits as Prelude
import Data.Char as Prelude
import Data.Complex as Prelude hiding (conjugate)
import Data.Containers.ListUtils as Prelude
import Data.Containers.NonEmpty as Prelude hiding (empty, pattern IsEmpty, pattern IsNonEmpty)
import Data.Data as Prelude
import Data.Either.Toolbox as Prelude (keepLeft, keepRight)
import Data.Foldable.Toolbox as Prelude (firstJust, firstJustM, notNull)
import Data.Function.Toolbox as Prelude hiding (applyN, (.:))
import Data.IntMap.NonEmpty as Prelude (NEIntMap)
import Data.IntSet.NonEmpty as Prelude (NEIntSet)
import Data.List.NonEmpty.Toolbox as Prelude (
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
import Data.List.Toolbox as Prelude (
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
import Data.Map.NonEmpty as Prelude (NEMap)
import Data.Monoid.Toolbox as Prelude hiding (discard, nomempty, option)
import Data.Sequence.NonEmpty as Prelude (NESeq)
import Data.Set.NonEmpty as Prelude (NESet)
import Data.Text as Prelude (strip)
import Data.Traversable as Prelude
import Data.Vector as Prelude (Vector)
import Geometry.Point as Prelude
import Geometry.Vector as Prelude hiding (Vector, head, init, last, replicate)
import Relude as Prelude hiding (First (..), Last (..), map, nonEmpty, (&&^), (||^))
import Relude.Extra.Enum as Prelude
import Relude.Extra.Foldable1 as Prelude
import Relude.Extra.Group as Prelude
import Relude.Extra.Map as Prelude
import Relude.Extra.Newtype as Prelude
import Relude.Extra.Tuple as Prelude
import Text.Parsec as Prelude hiding (State (..), choice, count, many, optional, parse, uncons, (<|>), pattern Empty)
import Text.Parsec.Text as Prelude (Parser)

applyN :: Natural -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN n f a = f $! applyN (pred n) f a

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap
