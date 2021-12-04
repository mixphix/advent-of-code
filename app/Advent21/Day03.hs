module Day03 where

base2P :: Parser Natural
base2P = do
  bits <- fmap (Const . relist) . many1 $ choice [0 <$ char '0', 1 <$ char '1']
  pure . undigits . getConst $ rebase @2 @10 bits

in03 :: [[Natural]]
in03 = map (parse number . one) . toString <$> lines (input 2021 3)

gamma :: [[Natural]] -> [Natural]
gamma = mapMaybe (viaNonEmpty (head . maximumOn1 length) . groupAll) . transpose

epsilon :: [[Natural]] -> [Natural]
epsilon = mapMaybe (viaNonEmpty (head . minimumOn1 length) . groupAll) . transpose

gammaKeep1 :: [[Natural]] -> [Natural]
gammaKeep1 =
  mapMaybe
    ( viaNonEmpty (head . maximumBy (liftM2 (<>) (comparing length) (comparing head)))
        . groupAll
    )
    . transpose

epsilonKeep0 :: [[Natural]] -> [Natural]
epsilonKeep0 =
  mapMaybe
    ( viaNonEmpty (head . minimumBy (liftM2 (<>) (comparing length) (comparing head)))
        . groupAll
    )
    . transpose

part1 :: Natural
part1 =
  let g = Const $ relist (gamma in03)
      e = Const $ relist (epsilon in03)
   in on (*) (undigits . getConst . rebase @2 @10) g e

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
  let o = Const $ relist (oxygen in03)
      c = Const $ relist (co2 in03)
   in on (*) (undigits . getConst . rebase @2 @10) o c
