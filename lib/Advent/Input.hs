module Advent.Input where

import GHC.IO (unsafePerformIO)
import System.FilePath ((<.>), (</>))

input :: Natural -> Natural -> Text
input (show -> year) (show -> day) = unsafePerformIO . readFileText $ "./input" </> year </> day <.> "txt"
