module Advent.Input where

import AdventAPI
import Data.Text (strip)
import GHC.IO (unsafePerformIO)
import System.Directory
import System.FilePath ((<.>), (</>))

input :: Natural -> Natural -> Text
input y@(show -> year) d@(show -> day) = unsafePerformIO $ do
  let inputfile = "./input" </> year </> day <.> "txt"
      aocopts =
        AoCOpts
          { _aSessionKey = "53616c7465645f5f48f11406b57218c1af2b45d64fdd6ad11aa1299f4502d4c23c1473baa0d7ed10c6ca05591b9c6743"
          , _aYear = fromIntegral y
          , _aCache = Nothing
          , _aForce = False
          , _aThrottle = 10 ^ (6 :: Int)
          }
  fmap strip . ifM (doesFileExist inputfile) (readFileText inputfile) $ do
    t <- runAoC_ aocopts (AoCInput . mkDay_ $ fromIntegral d)
    writeFileText inputfile t
    pure t
