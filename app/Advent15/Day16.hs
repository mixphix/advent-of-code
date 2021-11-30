{-# LANGUAGE FlexibleInstances #-}

module Day16 where

import Control.Lens (toListOf)
import Control.Lens.Extras (template)
import Data.Map.Strict qualified as Map

data Sue = Sue
  { children :: Maybe Int,
    cats :: Maybe Int,
    samoyeds :: Maybe Int,
    pomeranians :: Maybe Int,
    akitas :: Maybe Int,
    vizslas :: Maybe Int,
    goldfish :: Maybe Int,
    trees :: Maybe Int,
    cars :: Maybe Int,
    perfumes :: Maybe Int
  }
  deriving (Data)

instance Eq Sue where
  s1 == s2 = case (toListOf (template @Sue @(Maybe Int)) s1, toListOf (template @Sue @(Maybe Int)) s2) of
    (s1s, s2s) -> and $ zipWith (compMissing (==)) s1s s2s

compMissing :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
compMissing _ Nothing _ = True
compMissing _ _ Nothing = True
compMissing f (Just x) (Just y) = f x y

target :: Sue
target =
  Sue
    { children = Just 3,
      cats = Just 7,
      samoyeds = Just 2,
      pomeranians = Just 3,
      akitas = Just 0,
      vizslas = Just 0,
      goldfish = Just 5,
      trees = Just 3,
      cars = Just 2,
      perfumes = Just 1
    }

sueFromMap :: Map String Int -> Sue
sueFromMap m =
  Sue
    { children = m !? "children",
      cats = m !? "cats",
      samoyeds = m !? "samoyeds",
      pomeranians = m !? "pomeranians",
      akitas = m !? "akitas",
      vizslas = m !? "vizslas",
      goldfish = m !? "goldfish",
      trees = m !? "trees",
      cars = m !? "cars",
      perfumes = m !? "perfumes"
    }

sue :: Parser Sue
sue = do
  void $ string "Sue " *> number @Int <* string ": "
  sueFromMap <$> quantities
  where
    quantities :: Parser (Map String Int)
    quantities = do
      let l = map string ["children", "cats", "samoyeds", "pomeranians", "akitas", "vizslas", "goldfish", "trees", "cars", "perfumes"]
      foldr (uncurry Map.insert) Map.empty
        <$> ((,) <$> choice l <*> (string ": " *> number)) `sepBy1` string ", "

in16 :: [Sue]
in16 = mapMaybe (parsedWith sue) $ lines (input 2015 16)

part1 :: Int
part1 = maybe 0 succ $ elemIndex target in16

rangeSue :: Sue -> Bool
rangeSue s =
  and ([children, samoyeds, akitas, vizslas, cars, perfumes] <&> \f -> on (compMissing (==)) f s target)
    && and ([cats, trees] <&> \f -> on (compMissing (>)) f s target)
    && and ([pomeranians, goldfish] <&> \f -> on (compMissing (<)) f s target)

part2 :: Int
part2 = maybe 0 succ $ findIndex rangeSue in16
