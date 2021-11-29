module Advent (module Advent) where

import Advent.Coordinates as Advent
import Advent.D4 as Advent
import Advent.Functions as Advent
import Advent.Infinite as Advent
import Advent.Input as Advent
import Advent.Numbers as Advent
import Advent.Orphans as Advent
import Advent.Parsers as Advent
import Advent.Polynomials as Advent
import AdventAPI as Advent (Part (..))
import Control.Applicative as Advent
import Control.Arrow as Advent hiding (ArrowLoop (..), first, second)
import Control.Lens as Advent (at, ix, none, view, (%~), (.~), (<>~), (?~), (^.), _1, _2, _3, _4)
import Control.Monad as Advent
import Data.Char as Advent
import Data.Complex as Advent hiding (conjugate)
import Data.Containers.ListUtils as Advent
import Data.Containers.NonEmpty as Advent hiding (empty, nonEmpty, pattern IsEmpty, pattern IsNonEmpty)
import Data.Data as Advent
import Data.Either.Toolbox as Advent hiding ((+++), (|||))
import Data.Foldable.Toolbox as Advent hiding (elem, genericLength, notElem, product, sum, toList)
import Data.Function.Toolbox as Advent hiding (applyN, (.:))
import Data.Function.Toolbox qualified (applyN)
import Data.Geometry.Point as Advent
import Data.Geometry.Vector (Vector)
import Data.Geometry.Vector as Advent hiding (Vector, head, last, replicate)
import Data.List.NonEmpty.Toolbox as Advent ((<|), (<||), (|>), (||>))
import Data.Map.Monoidal.Strict as Advent ((!))
import Data.Map.Monoidal.Strict qualified as Mop (Map)
import Data.Traversable as Advent
import Data.Vector as Advent (Vector)
import GHC.Exts as Advent (IsList (fromList))
import Relude.Extra.Enum as Advent
import Relude.Extra.Group as Advent
import Relude.Extra.Map as Advent
import Relude.Extra.Newtype as Advent
import Relude.Extra.Tuple as Advent
import Text.Parsec as Advent hiding (choice, count, many, optional, uncons, (<|>), pattern Empty)
import Text.Parsec.Text as Advent (Parser)

applyN :: Int -> (a -> a) -> a -> a
applyN = Data.Function.Toolbox.applyN @Int

type Mop = Mop.Map

type V = Data.Geometry.Vector.Vector
