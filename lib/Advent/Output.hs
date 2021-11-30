{-# LANGUAGE TemplateHaskell #-}

module Advent.Output where

import Advent
import Data.Foldable (maximum)
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH
import Numeric (showFFloat)
import System.Directory (listDirectory)
import System.FilePath (stripExtension)

seconds :: NominalDiffTime -> String
seconds t = showFFloat (Just 3) (realToFrac @_ @Double t) []

milliseconds :: NominalDiffTime -> String
milliseconds t = showFFloat (Just 3) (1000 * realToFrac @_ @Double t) []

mainFor :: Natural -> Q Exp
mainFor yr =
  doE
    [ bindS (varP $ mkName "ts") $ appE (appE [|fmap|] [|systemToUTCTime|]) [|getSystemTime|],
      bindS (varP $ mkName "args") [|getArgs|],
      letS [valD (varP $ mkName "k") (normalB [|(readMaybe <=< viaNonEmpty head) args ?: 1|]) []],
      noBindS $ appE [|for_ [k .. maxDay yr]|] (mainHelper yr),
      bindS (varP $ mkName "te") $ appE (appE [|fmap|] [|systemToUTCTime|]) [|getSystemTime|],
      letS [valD (varP $ mkName "t") (normalB [|diffUTCTime te ts|]) []],
      noBindS $ appE [|putStrLn|] [|"Total time: " <> seconds t <> " seconds"|]
    ]

maxDay :: Natural -> Natural
{-# NOINLINE maxDay #-}
maxDay yr = unsafePerformIO $ do
  let [_, _, y1, y2] = relist $ digits yr
      advent = "./app/Advent" <> show (10 * y1 + y2)
  files <- mapMaybe (T.stripPrefix "Day" . toText <=< stripExtension "hs") <$> listDirectory advent
  pure $ maximum (parsedWith number <$> files) ?: 0

mainHelper :: Natural -> Q Exp
mainHelper yr =
  lamCaseE $
    [1 .. maxDay yr] <&> \d ->
      match (litP $ IntegerL (fromIntegral d)) (normalB $ day d) []

daypart :: Natural -> Natural -> Q Exp
daypart d n = pure . VarE . mkName $ "Day" <> padWith 2 '0' d <> ".part" <> show n

day :: Natural -> Q Exp
day d =
  doE $
    [ noBindS $ appE [|putStrLn|] [|"Day " <> dstr <> ""|],
      noBindS $ appE [|putStr|] [|"  Part 1:"|],
      bindS (varP $ td "'1") $ appE (appE [|fmap|] [|systemToUTCTime|]) [|getSystemTime|],
      noBindS $ appE [|putStr . padWith 48 ' '|] (daypart d 1),
      bindS (varP $ td "'2") $ appE (appE [|fmap|] [|systemToUTCTime|]) [|getSystemTime|],
      letS [valD (varP $ td "'") (normalB $ appE (appE [|diffUTCTime|] (varE $ td "'2")) (varE $ td "'1")) []],
      noBindS . appE [|putStrLn|] $ appE [|("  (" <>) . (<> " ms)")|] (ms . varE $ td "'")
    ]
      ++ if d /= 25
        then
          [ noBindS $ appE [|putStr|] [|"  Part 2:"|],
            bindS (varP $ td "'3") $ appE (appE [|fmap|] [|systemToUTCTime|]) [|getSystemTime|],
            noBindS $ appE [|putStr . padWith 48 ' '|] (daypart d 2),
            bindS (varP $ td "'4") $ appE (appE [|fmap|] [|systemToUTCTime|]) [|getSystemTime|],
            letS [valD (varP $ td "''") (normalB $ appE (appE [|diffUTCTime|] (varE $ td "'4")) (varE $ td "'3")) []],
            noBindS . appE [|putStrLn|] $ appE [|("  (" <>) . (<> " ms)")|] (ms . varE $ td "''")
          ]
        else []
  where
    dstr = padWith 2 '0' d
    td k = mkName $ "t" <> dstr <> k
    ms = appE [|paddedWith 10 ' ' . milliseconds|]
