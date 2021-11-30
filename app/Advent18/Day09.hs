module Day09 where

import Advent
import Control.Lens ((+~))
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector

type Elf = Int

type Marble = Int

type Scoreboard = Map Elf Int

data Circle = Circle {contents :: NEVector Int, current :: Int} deriving (Eq, Show)

cvec :: Circle -> NEVector Int
cvec Circle {..} =
  let v = relist contents
      v' = Vector.take (length contents) $ NEVector.drop current (v <> v)
   in relist v'

in09 :: (Elf, Marble)
players :: Elf
himarble :: Marble
in09@(players, himarble) =
  parsedWith ((,) <$> number <*> (string " players; last marble is worth " *> number)) (input 2018 9) ?: (0, 0)

elves :: Scoreboard
elves = relist $ [1 .. players] <&> (,0)

circle :: Circle
circle = Circle (one 0) 0

insertAt :: Int -> Int -> Circle -> Circle
insertAt k0 a c =
  let v = cvec c
      k = (current c + k0) `mod` length v
      v' = NEVector.take k v <> one a <> NEVector.drop k v
   in Circle v' k

deleteAt :: Int -> Circle -> (Int, Circle)
deleteAt k0 c =
  let v = cvec c
      k = (current c + k0) `mod` length v
      Just v' = (liftM2 (<>) `on` nonEmpty) (NEVector.take k v) (NEVector.drop (k + 1) v)
   in (v NEVector.! k, Circle v' k)

place :: Elf -> Marble -> (Scoreboard, Circle) -> (Scoreboard, Circle)
place e m (s, c)
  | multipleOf 23 m =
    let (m', c') = deleteAt (-7) c
     in (s & ix e +~ (m + m'), c')
  | otherwise = (s, insertAt 1 m c)

part1 :: Int
part1 =
  maximum
    . fst
    . foldl' (flip $ uncurry place) (elves, circle)
    $ zip (cycle [1 .. players]) [1 .. himarble]
