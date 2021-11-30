module Day21 where

import Data.Text qualified as T

data Weapon
  = Dagger
  | Shortsword
  | Warhammer
  | Longsword
  | Greataxe
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Armour
  = Leather
  | Chainmail
  | Splintmail
  | Bandedmail
  | Platemail
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Ring
  = DMG1
  | DMG2
  | DMG3
  | DEF1
  | DEF2
  | DEF3
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Equipment
  = Weap Weapon
  | Arm (Maybe Armour)
  | Rng (Maybe (Ring, Maybe Ring))
  deriving (Eq, Ord, Show, Read)

cost :: Equipment -> Natural
cost = \case
  Weap w -> case w of
    Dagger -> 8
    Shortsword -> 10
    Warhammer -> 25
    Longsword -> 40
    Greataxe -> 74
  Arm Nothing -> 0
  Arm (Just a) -> case a of
    Leather -> 13
    Chainmail -> 31
    Splintmail -> 53
    Bandedmail -> 75
    Platemail -> 102
  Rng Nothing -> 0
  Rng (Just (r, Nothing)) -> case r of
    DMG1 -> 25
    DMG2 -> 50
    DMG3 -> 100
    DEF1 -> 20
    DEF2 -> 40
    DEF3 -> 80
  Rng (Just (r, Just r')) -> cost (Rng (Just (r, Nothing))) + cost (Rng (Just (r', Nothing)))

damage :: Equipment -> Natural
damage = \case
  Weap w -> case w of
    Dagger -> 4
    Shortsword -> 5
    Warhammer -> 6
    Longsword -> 7
    Greataxe -> 8
  Arm _ -> 0
  Rng Nothing -> 0
  Rng (Just (r, Nothing)) -> case r of
    DMG1 -> 1
    DMG2 -> 2
    DMG3 -> 3
    _ -> 0
  Rng (Just (r, Just r')) -> damage (Rng (Just (r, Nothing))) + damage (Rng (Just (r', Nothing)))

armour :: Equipment -> Natural
armour = \case
  Weap _ -> 0
  Arm Nothing -> 0
  Arm (Just a) -> case a of
    Leather -> 1
    Chainmail -> 2
    Splintmail -> 3
    Bandedmail -> 4
    Platemail -> 5
  Rng Nothing -> 0
  Rng (Just (r, Nothing)) -> case r of
    DEF1 -> 1
    DEF2 -> 2
    DEF3 -> 3
    _ -> 0
  Rng (Just (r, Just r')) -> armour (Rng (Just (r, Nothing))) + armour (Rng (Just (r', Nothing)))

data Player = Player
  { hp :: Natural,
    dmg :: Natural,
    def :: Natural
  }
  deriving (Eq, Ord, Show)

smith, armoury, jeweller :: [Equipment]
(smith, armoury, jeweller) =
  ( Weap <$> universe,
    Arm <$> Nothing : map Just universe,
    map Rng $
      Nothing :
      map (Just . (,Nothing)) universe
        <> [Just (r, Just r') | r <- universe, r' <- universe \\ [r]]
  )

spend :: Natural -> [[Equipment]]
spend n = do
  w <- smith
  a <- armoury
  r <- jeweller
  guarded ((n ==) . sumOn cost) [w, a, r]

maxSpend :: Natural
maxSpend = sumOn cost [Weap Greataxe, Arm (Just Platemail), Rng (Just (DMG3, Just DEF3))]

attack, defense :: [Equipment] -> Natural
attack = sumOn damage
defense = sumOn armour

humans :: Natural -> Set Player
humans n = relist $ uncurry (Player 100) . (attack &&& defense) <$> spend n

boss :: Player
boss =
  let [h, d, a] = parse number . T.filter isDigit <$> lines (input 2015 21)
   in Player h d a

data Turn = Mine | Yours deriving (Eq, Show)

hit :: Player -> Player -> Maybe Player
a `hit` b = points < hp b ? b {hp = hp b - points}
  where
    points = case dmg a `compare` def b of
      GT -> dmg a - def b
      _ -> 1

fight :: Turn -> Player -> Player -> Bool
fight Mine you me = case me `hit` you of
  Nothing -> True
  Just you' -> fight Yours you' me
fight Yours you me = case you `hit` me of
  Nothing -> False
  Just me' -> fight Mine you me'

win :: Player -> Bool
win = fight Mine boss

part1 :: Natural
part1 = find (any win . humans) [0 .. maxSpend] ?: 0

part2 :: Natural
part2 = withNonEmpty 0 last $ filter (not . all win . humans) [0 .. maxSpend]
