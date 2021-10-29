module Day21 where

import Advent hiding (D4 (..))
import Data.List.Toolbox ((\\))
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
  = W Weapon
  | A (Maybe Armour)
  | R (Maybe (Ring, Maybe Ring))
  deriving (Eq, Ord, Show, Read)

cost :: Equipment -> Natural
cost = \case
  W w -> case w of
    Dagger -> 8
    Shortsword -> 10
    Warhammer -> 25
    Longsword -> 40
    Greataxe -> 74
  A Nothing -> 0
  A (Just a) -> case a of
    Leather -> 13
    Chainmail -> 31
    Splintmail -> 53
    Bandedmail -> 75
    Platemail -> 102
  R Nothing -> 0
  R (Just (r, Nothing)) -> case r of
    DMG1 -> 25
    DMG2 -> 50
    DMG3 -> 100
    DEF1 -> 20
    DEF2 -> 40
    DEF3 -> 80
  R (Just (r, Just r')) -> cost (R (Just (r, Nothing))) + cost (R (Just (r', Nothing)))

damage :: Equipment -> Natural
damage = \case
  W w -> case w of
    Dagger -> 4
    Shortsword -> 5
    Warhammer -> 6
    Longsword -> 7
    Greataxe -> 8
  A _ -> 0
  R Nothing -> 0
  R (Just (r, Nothing)) -> case r of
    DMG1 -> 1
    DMG2 -> 2
    DMG3 -> 3
    _ -> 0
  R (Just (r, Just r')) -> damage (R (Just (r, Nothing))) + damage (R (Just (r', Nothing)))

armour :: Equipment -> Natural
armour = \case
  W _ -> 0
  A Nothing -> 0
  A (Just a) -> case a of
    Leather -> 1
    Chainmail -> 2
    Splintmail -> 3
    Bandedmail -> 4
    Platemail -> 5
  R Nothing -> 0
  R (Just (r, Nothing)) -> case r of
    DEF1 -> 1
    DEF2 -> 2
    DEF3 -> 3
    _ -> 0
  R (Just (r, Just r')) -> armour (R (Just (r, Nothing))) + armour (R (Just (r', Nothing)))

data Player = Player
  { hp :: Natural,
    dmg :: Natural,
    def :: Natural
  }
  deriving (Eq, Ord, Show)

smith, armoury, jeweller :: [Equipment]
(smith, armoury, jeweller) =
  ( W <$> universe,
    A <$> Nothing : map Just universe,
    map R $
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
maxSpend = sumOn cost [W Greataxe, A (Just Platemail), R (Just (DMG3, Just DEF3))]

attack, defense :: [Equipment] -> Natural
attack = sumOn damage
defense = sumOn armour

humans :: Natural -> Set Player
humans n = relist $ uncurry (Player 100) . (attack &&& defense) <$> spend n

boss :: Player
boss =
  let [h, d, a] = mapMaybe (parsedWith number . T.filter isDigit) $ lines (input 2015 21)
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
