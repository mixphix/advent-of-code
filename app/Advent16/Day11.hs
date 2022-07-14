module Day11 where

import Control.Lens (Lens', lens)
import Data.Set (powerSet)
import Data.Set qualified as Set
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
    [ Pm <$ string "promethium"
    , Co <$ string "cobalt"
    , Cu <$ string "curium"
    , Ru <$ string "ruthenium"
    , Pu <$ string "plutonium"
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
  g <-
    choice
      [ Chip <$ string "-compatible microchip"
      , RTG <$ string " generator"
      ]
  pure $ g i

data Floor = One | Two | Three | Four deriving (Eq, Ord, Enum, Bounded, Show)

gizmoFloor :: Parser (Floor, [Gizmo])
gizmoFloor = do
  f <-
    string "The "
      *> choice
        [ One <$ string "first"
        , Two <$ string "second"
        , Three <$ string "third"
        , Four <$ string "fourth"
        ]
  gs <- fmap (?: []) . optional . try $ do
    void $ string " floor contains"
    gs <- many . try $ string " a " *> gizmo <* optional (string ",")
    g <- string " and a " *> gizmo
    pure (g : gs)
  pure (f, gs)

irradiate :: [Gizmo] -> Maybe [Gizmo]
irradiate = guarded $ (null . gens) ||^ (null . liftM2 (\\) chips gens)

data Area = Area
  { me :: Floor
  , floors :: Mop Floor (Set Gizmo)
  }
  deriving (Eq, Ord)

instance Show Area where
  show Area{..} =
    let t = show . relist @_ @[] . (floors !) <$> [One .. Four]
     in toString . unlines $ t & ix (fromEnum me) <>~ "<"

in11 :: Area
in11 =
  Area One . map relist . relist $
    parse gizmoFloor <$> lines (input 2016 11)

transformArea :: (Isotope, Isotope) -> Area -> Area
transformArea is Area{..} = Area me $ Set.map (transform is) <$> floors

equivalents :: Area -> Set Area
equivalents =
  relist . flap [transformArea (i, j) | i <- universe, j <- universe \\ [i]]

equivalent :: Area -> Area -> Bool
equivalent a b = a == b || a `member` equivalents b

end :: Area -> Bool
end Area{..} =
  me == Four && floors ! Four == fold [relist [Chip i, RTG i] | i <- universe]

move :: Bool -> Set Gizmo -> Area -> Maybe (Area, Set Gizmo)
move up gizmos Area{..}
  | up && me == Four = Nothing
  | not up && me == One = Nothing
  | size gizmos `notElem` [1, 2] = Nothing
  | otherwise =
    let me' = bool pred succ up me
        floors' =
          floors
            & ix me %~ (`Set.difference` gizmos)
            & ix me' <>~ gizmos
     in case length . mapMaybe (irradiate . relist) $ elems floors' of
          4 -> Just (Area me' floors', gizmos)
          _ -> Nothing

moves :: Area -> Maybe [(Area, Set Gizmo)]
moves area | end area = Nothing
moves area@Area{..} =
  let available =
        relist @_ @[]
          . Set.filter ((<= 2) . length)
          . powerSet
          $ relist (floors ! me)
   in Just . mapMaybe ($ area) $ move <$> universe <*> available

unfold :: (a -> b -> Maybe [(a, b)]) -> a -> NonEmpty b -> [b]
unfold f a (b :| bs) = case f a b of
  Nothing -> bs
  Just x0 -> _

part2 :: Natural
part2 = 0
