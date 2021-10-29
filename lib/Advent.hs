module Advent (module Advent) where

import Advent.Coordinates as Advent
import Advent.D4 as Advent
import Advent.Functions as Advent
import Advent.Infinite as Advent
import Advent.Input as Advent
import Advent.Numbers as Advent
import Advent.Orphans as Advent ()
import Advent.Parsers as Advent
import Advent.Polynomials as Advent
import Control.Lens as Advent (none, view, (^.), _1, _2, _3, _4)
import Control.Monad.Toolbox as Advent hiding (guarded, (<<$>>))
import Data.Char as Advent
import Data.Complex as Advent hiding (conjugate)
import Data.Containers.NonEmpty as Advent hiding (empty, nonEmpty)
import Data.Data as Advent
import Data.Foldable.Toolbox as Advent hiding (elem, genericLength, notElem, product, sum, toList)
import Data.List.NonEmpty.Toolbox as Advent ((<|), (<||), (|>), (||>))
import Data.Traversable as Advent
import GHC.Exts as Advent (IsList (..))
import Linear as Advent hiding (E)
import Relude.Extra.Map as Advent
import Text.Parsec as Advent hiding (choice, count, many, optional, uncons, (<|>), pattern Empty)
import Text.Parsec.Text as Advent (Parser)

data Part = Part1 | Part2 deriving (Eq, Ord, Show)
