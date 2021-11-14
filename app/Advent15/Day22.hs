{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import Advent
import Control.Lens (makeLenses, (+~), (-~), (.~))
import Data.List.NonEmpty.Toolbox (minimumOf1)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

data Player = Player
  { _health :: Natural,
    _dmg :: Natural,
    _def :: Natural,
    _mana :: Natural
  }
  deriving (Eq, Ord, Show)

makeLenses ''Player

data Spell
  = Missile
  | Drain
  | Shield
  | Poison
  | Charge
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

cost :: Spell -> Natural
cost = \case
  Missile -> 53
  Drain -> 73
  Shield -> 113
  Poison -> 173
  Charge -> 229

damage :: Spell -> Natural
damage = \case
  Missile -> 4
  Drain -> 2
  Shield -> 0
  Poison -> 0
  Charge -> 0

heal :: Spell -> Natural
heal = \case
  Missile -> 0
  Drain -> 2
  Shield -> 0
  Poison -> 0
  Charge -> 0

data Effect = S | P | C deriving (Eq, Ord, Enum, Bounded, Show, Read)

spellEffect :: Spell -> Maybe (Effect, Natural)
spellEffect = \case
  Shield -> Just (S, 6)
  Poison -> Just (P, 6)
  Charge -> Just (C, 5)
  _ -> Nothing

effectDamage :: Effect -> Natural
effectDamage = \case
  P -> 3
  _ -> 0

effectStatus :: Effect -> Player -> Player
effectStatus = \case
  S -> (def .~ 7)
  C -> (mana +~ 101)
  _ -> id

effectuate :: Map Effect Natural -> Map Effect Natural
effectuate = Map.foldMapWithKey (\eff dur -> if dur > 1 then one (eff, pred dur) else Map.empty)

boss :: Player
boss =
  let [h, d] = mapMaybe (parsedWith number . T.filter isDigit) $ lines (input 2015 22)
   in Player h d 0 0

data Turn = Mine | Yours deriving (Eq, Show)

hit :: Natural -> Player -> Maybe Player
0 `hit` b = Just b
a `hit` b = points < _health b ? (health -~ points) b
  where
    points = case a `compare` _def b of
      GT -> a - _def b
      _ -> 1

fight :: Part -> Turn -> Map Effect Natural -> Player -> Player -> Spell -> Alt [] [Spell]
fight diff Yours effects0 you0 me0 spell =
  let active = keys effects0
      (buffs, debuffs) = unzip $ (effectStatus &&& effectDamage) <$> active
      effects = effectuate effects0
      me1 = foldl' (&) me0 $ bool ((def .~ 0) :) id (S `elem` active) buffs
   in case hit (sum debuffs) you0 of
        Nothing -> pure []
        Just you -> case hit (you ^. dmg) me1 of
          Nothing -> empty
          Just me -> fight diff Mine effects you me spell
fight Part1 Mine effects0 you0 me0 spell =
  let active = keys effects0
      (buffs, debuffs) = unzip $ (effectStatus &&& effectDamage) <$> active
      effects = effectuate effects0
      hp = heal spell
      me1 = foldl' (&) me0 $ bool ((def .~ 0) :) id (S `elem` active) buffs
      mp = cost spell
   in case hit (sum debuffs) you0 of
        Nothing -> pure []
        Just you1
          | me1 ^. mana >= mp ->
            (spell :) <$> case hit (damage spell) you1 of
              Nothing -> pure []
              Just you ->
                let meff = spellEffect spell
                    me =
                      me1
                        & mana -~ mp
                        & health +~ hp
                 in case meff of
                      Nothing -> universe >-< fight Part1 Yours effects you me
                      Just (e, d)
                        | Just _ <- effects !? e -> empty
                        | otherwise ->
                          universe >-< fight Part1 Yours (insert e d effects) you me
          | otherwise -> empty
fight Part2 Mine effects0 you0 me0 spell =
  let active = keys effects0
      (buffs, debuffs) = unzip $ (effectStatus &&& effectDamage) <$> active
      effects = effectuate effects0
      hp = heal spell
      me1 = foldl' (&) me0 $ bool ((def .~ 0) :) id (S `elem` active) buffs
      mp = cost spell
   in case hit 1 me1 of
        Nothing -> empty
        Just me2 -> case hit (sum debuffs) you0 of
          Nothing -> pure []
          Just you1
            | me2 ^. mana >= mp ->
              (spell :) <$> case hit (damage spell) you1 of
                Nothing -> pure []
                Just you ->
                  let meff = spellEffect spell
                      me =
                        me2
                          & mana -~ mp
                          & health +~ hp
                   in case meff of
                        Nothing -> universe >-< fight Part2 Yours effects you me
                        Just (e, d)
                          | Just _ <- effects !? e -> empty
                          | otherwise ->
                            universe >-< fight Part2 Yours (insert e d effects) you me
            | otherwise -> empty

part1 :: Natural
part1 =
  withNonEmpty 0 (minimumOf1 (sumOn cost)) . getAlt $
    universe >-< fight Part1 Mine Map.empty boss (Player 50 0 0 500)

part2 :: Natural
part2 =
  withNonEmpty 0 (minimumOf1 (sumOn cost)) . getAlt $
    universe >-< fight Part2 Mine Map.empty boss (Player 50 0 0 500)
