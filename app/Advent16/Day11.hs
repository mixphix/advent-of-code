module Day11 where

import Advent
import Control.Lens (Ixed (ix), Lens', lens, (.~), (<>~))
import Control.Monad.ST
import Data.List.Toolbox ((\\))
import Data.Monoid.Toolbox
import Data.STRef
import Text.Show qualified (show)

data Isotope
  = Pm
  | Co
  | Cu
  | Ru
  | Pu
  deriving (Eq, Ord, Enum, Bounded, Show)

isotopes :: Parser Isotope
isotopes =
  choice
    [ Pm <$ string "promethium",
      Co <$ string "cobalt",
      Cu <$ string "curium",
      Ru <$ string "ruthenium",
      Pu <$ string "plutonium"
    ]

data Gizmo
  = RTG Isotope
  | Chip Isotope
  deriving (Eq, Ord, Show)

isotope :: Lens' Gizmo Isotope
isotope =
  lens
    ( \case
        Chip i -> i
        RTG i -> i
    )
    ( \case
        Chip _ -> Chip
        RTG _ -> RTG
    )

chips, gens :: [Gizmo] -> [Isotope]
chips g = [c | Chip c <- g]
gens g = [c | RTG c <- g]

transform :: (Isotope, Isotope) -> Gizmo -> Gizmo
transform (i, j) g
  | g ^. isotope == i = g & isotope .~ j
  | g ^. isotope == j = g & isotope .~ i
  | otherwise = g

gizmo :: Parser Gizmo
gizmo = do
  i <- isotopes
  g <- choice [Chip <$ string "-compatible microchip", RTG <$ string " generator"]
  pure $ g i

data Floor = One | Two | Three | Four deriving (Eq, Ord, Enum, Bounded, Show)

gizmoFloor :: Parser (Floor, [Gizmo])
gizmoFloor = do
  f <-
    string "The "
      *> choice
        [ One <$ string "first",
          Two <$ string "second",
          Three <$ string "third",
          Four <$ string "fourth"
        ]
  gs <- fmap (?: []) . optional . try $ do
    void $ string " floor contains"
    gs <- many . try $ string " a " *> gizmo <* optional (string ",")
    g <- string " and a " *> gizmo
    pure (g : gs)
  pure (f, gs)

in11 :: Map Floor [Gizmo]
in11 = relist . mapMaybe (parsedWith gizmoFloor) $ lines (input 2016 11)

irradiate :: [Gizmo] -> Maybe [Gizmo]
irradiate = guarded $ (null . gens) ||^ (null . liftM2 (\\) chips gens)

newtype Floors = Floors {unFloors :: Map Floor [Gizmo]}
  deriving (Eq)

instance Show Floors where
  show (Floors m) =
    toString (unlines . map show $ mapMaybe (m !?) [One .. Four])

data Area = Area {me :: Floor, floors :: Floors}
  deriving (Eq)

instance Show Area where
  show Area {..} =
    let t = lines $ show floors
     in toString . unlines $ t & ix (fromEnum me) <>~ "<"

transformArea :: (Isotope, Isotope) -> Area -> Area
transformArea is a = Area (me a) $ Floors (unFloors (floors a) <&> sort . map (transform is))

equivalent :: Area -> Area -> Bool
equivalent a b =
  a == b
    || let equivs = [transformArea (i, j) | i <- universe, j <- universe \\ [i]] ?? a
        in Area (me b) (Floors $ unFloors (floors b) <&> sort) `elem` equivs

end :: Area -> Bool
end Area {..} = me == Four && sort (unFloors floors !? Four ?: []) == sort (fold [[Chip i, RTG i] | i <- universe])

moves :: Area -> [(Floor, [[Gizmo]])]
moves Area {..} =
  let at 1 = maybe [] (map one . sort) (unFloors floors !? me)
      at 2 = case unFloors floors !? me ?: [] of
        [] -> []
        [_] -> []
        _ -> nubOrd . map toList $ pairsTied (sort $ unFloors floors !? me ?: [])
      at (_ :: Word8) = []
   in case me of
        One -> [(Two, at 2 <> at 1)]
        Four -> [(Three, at 1 <> at 2)]
        n -> [(succ n, at 2 <> at 1)] <> [(pred n, at 1 <> at 2)]

move :: forall s. STRef s [Area] -> Natural -> Area -> [(Floor, [[Gizmo]])] -> ST s (Maybe (Min Natural))
move _ _ _ [] = pure empty
move as n a ((_, []) : ms) = move as n a ms
move as n a0@(Area {..}) ((e, gs : gizmos) : ms) = do
  seen <- readSTRef as
  let a' = do
        f <- unFloors floors !? me
        newoldfloor <- irradiate $ f \\ gs
        let removed = alter (newoldfloor <$) me $ unFloors floors
        f' <- removed !? e
        newnewfloor <- irradiate $ f' <> gs
        pure . traceShowId . Area e . Floors $ alter (newnewfloor <$) e removed
  case a' of
    Nothing -> pure Nothing
    Just a@Area {}
      | any (equivalent a) seen -> pure Nothing
      | end a -> pure . Just $ Min n
      | otherwise ->
        liftM2 (<|>) (move as n a0 ((e, gizmos) : ms)) $ do
          modifySTRef' as (a :)
          move as (succ n) a (moves a)

part1 :: Natural
part1 =
  let a = Area One (Floors in11)
   in runST $ do
        seen <- newSTRef []
        maybe 0 getMin <$> move seen 0 a (moves a)

part2 :: Natural
part2 = 0
