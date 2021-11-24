module Advent.Input where

import AdventAPI
import GHC.IO (unsafePerformIO)
import System.Directory
import System.FilePath ((<.>), (</>))

input :: Natural -> Natural -> Text
input (show -> year) (show -> day) = unsafePerformIO $ do
  let inputfile = "./input" </> year </> day <.> "txt"
      aocopts = AoCOpts {_aSessionKey = "53616c7465645f5fc17036fcdfe1e13dbbc124e1151a6fa12c2af184ebaf0bc272988346ece9d9f382a1b142d580dbb2"
      }
  ifM (doesFileExist inputfile) (readFileText inputfile) $ do
    pure ()
