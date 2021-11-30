module Day09 where

import Control.Lens ((+~))
import Data.Sequence qualified as Seq

type Elf = Int

type Marble = Int

type Scoreboard = NEMap Elf Int

data Circle = Circle {contents :: Seq Int, current :: Int} deriving (Eq, Show)

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
  let v = contents c
      k = (current c + k0) `mod` length v
   in Circle (Seq.insertAt k a v) k

deleteAt :: Int -> Circle -> (Int, Circle)
deleteAt k0 c =
  let v = contents c
      k = (current c + k0) `mod` length v
      v' = Seq.deleteAt k v
   in (v `Seq.index` k, Circle v' k)

place :: Elf -> Marble -> (Scoreboard, Circle) -> (Scoreboard, Circle)
place e m (s, c)
  | multipleOf 23 m =
    let (m', c') = deleteAt (-7) c
     in (s & ix e +~ (m + m'), c')
  | otherwise = (s, insertAt 1 m c)

part1 :: Int
part1 =
  maximum1
    . fst
    . foldl' (flip $ uncurry place) (elves, circle)
    $ zip (cycle [1 .. players]) [1 .. himarble]