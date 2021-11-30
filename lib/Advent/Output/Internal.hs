{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Advent.Output.Internal where

import Advent.Functions (padWith, relist)
import Advent.Numbers (digits)
import Advent.Parsers (number, parse)
import Data.List (maximum)
import Data.Text qualified as T
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH
import System.Directory (listDirectory)
import System.FilePath (stripExtension)

maxDay :: Natural -> Natural
{-# NOINLINE maxDay #-}
maxDay yr = unsafePerformIO $ do
  let [_, _, y1, y2] = relist $ digits yr
      advent = "./app/Advent" <> show (10 * y1 + y2)
  files <- mapMaybe (T.stripPrefix "Day" . toText <=< stripExtension "hs") <$> listDirectory advent
  pure $ maximum (parse number <$> files) ?: 0

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
      noBindS $ appE [|putStr . padWith 16 ' '|] (daypart d 1),
      bindS (varP $ td "'2") $ appE (appE [|fmap|] [|systemToUTCTime|]) [|getSystemTime|],
      letS [valD (varP $ td "'") (normalB $ appE (appE [|diffUTCTime|] (varE $ td "'2")) (varE $ td "'1")) []],
      noBindS . appE [|putStrLn|] $ appE [|("  (" <>) . (<> " ms)")|] (ms . varE $ td "'")
    ]
      ++ if d /= 25
        then
          [ noBindS $ appE [|putStr|] [|"  Part 2:"|],
            bindS (varP $ td "'3") $ appE (appE [|fmap|] [|systemToUTCTime|]) [|getSystemTime|],
            noBindS $ appE [|putStr . padWith 16 ' '|] (daypart d 2),
            bindS (varP $ td "'4") $ appE (appE [|fmap|] [|systemToUTCTime|]) [|getSystemTime|],
            letS [valD (varP $ td "''") (normalB $ appE (appE [|diffUTCTime|] (varE $ td "'4")) (varE $ td "'3")) []],
            noBindS . appE [|putStrLn|] $ appE [|("  (" <>) . (<> " ms)")|] (ms . varE $ td "''")
          ]
        else []
  where
    dstr = padWith 2 '0' d
    td k = mkName $ "t" <> dstr <> k
    ms = appE [|paddedWith 10 ' ' . milliseconds|]
