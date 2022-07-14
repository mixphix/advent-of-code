module Day03 where

import Data.Text qualified as T

base2P :: Parser Natural
base2P = bits . relist <$> many1 (choice [0 <$ char '0', 1 <$ char '1'])

in03 :: [[Natural]]
in03 = map (parse number . T.singleton) . toString <$> lines (input 2021 3)

gamma :: [[Natural]] -> [Natural]
gamma =
  mapMaybe (viaNonEmpty (head . maximumOn1 length) . groupAll) . transpose

epsilon :: [[Natural]] -> [Natural]
epsilon =
  mapMaybe (viaNonEmpty (head . minimumOn1 length) . groupAll) . transpose

gammaKeep1 :: [[Natural]] -> [Natural]
gammaKeep1 =
  mapMaybe
    ( viaNonEmpty
        (head . maximumBy (liftM2 (<>) (comparing length) (comparing head)))
        . groupAll
    )
    . transpose

epsilonKeep0 :: [[Natural]] -> [Natural]
epsilonKeep0 =
  mapMaybe
    ( viaNonEmpty
        (head . minimumBy (liftM2 (<>) (comparing length) (comparing head)))
        . groupAll
    )
    . transpose

bits :: NESeq Natural -> Natural
bits = undigits . getConst . rebase @2 @10 . Const

part1 :: Natural
part1 =
  let g = relist (gamma in03)
      e = relist (epsilon in03)
   in bits g * bits e

bitcrit :: ([[Natural]] -> [Natural]) -> Natural -> [[Natural]] -> [Natural]
bitcrit _ _ [o] = o
bitcrit scrub n os =
  let Just g = scrub os !!? fromIntegral n
      os' = filter ((Just g ==) . (!!? fromIntegral n)) os
   in bitcrit scrub (succ n) os'

oxygen :: [[Natural]] -> [Natural]
oxygen = bitcrit gammaKeep1 0

co2 :: [[Natural]] -> [Natural]
co2 = bitcrit epsilonKeep0 0

part2 :: Natural
part2 =
  let o = relist (oxygen in03)
      c = relist (co2 in03)
   in bits o * bits c
