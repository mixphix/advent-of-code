module Day06 where

import Data.Map.Monoidal.Strict qualified as Mop
import Data.Text qualified as T

in06 :: Mop Natural (Sum Natural)
in06 = foldMap (@= 1) $ parse number <$> T.splitOn "," (input 2021 6)

timers :: Mop Natural (Sum Natural) -> Mop Natural (Sum Natural)
timers m0 =
  let z = m0 ! 0
      m = Mop.mapKeys pred $ m0 `Mop.withoutKeys` one 0
   in m <> (6 @= z) <> (8 @= z)

part1 :: Natural
part1 = getSum . maybe 0 fold $ iterate' timers in06 !!? 80

part2 :: Natural
part2 = getSum . maybe 0 fold $ iterate' timers in06 !!? 256
