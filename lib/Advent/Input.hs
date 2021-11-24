module Advent.Input where

import AdventAPI
import GHC.IO (unsafePerformIO)
import System.Directory
import System.FilePath ((<.>), (</>))

input :: Natural -> Natural -> Text
input y@(show -> year) d@(show -> day) = unsafePerformIO $ do
  let inputfile = "./input" </> year </> day <.> "txt"
      aocopts =
        AoCOpts
          { _aSessionKey = "53616c7465645f5fc17036fcdfe1e13dbbc124e1151a6fa12c2af184ebaf0bc272988346ece9d9f382a1b142d580dbb2",
            _aYear = fromIntegral y,
            _aCache = Nothing,
            _aForce = False,
            _aThrottle = 10 ^ (6 :: Int)
          }
  ifM (doesFileExist inputfile) (readFileText inputfile) $ do
    t <- runAoC_ aocopts (AoCInput . mkDay_ $ fromIntegral d)
    writeFileText inputfile t
    pure t
