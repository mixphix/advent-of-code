module Day08 where

import Data.Text qualified as T
import Prelude hiding (C, E)

in08 :: [[Text]]
in08 = words <$> lines (input 2021 8)

part1 :: Natural
part1 =
  let outputvals = drop 1 . dropWhile (/= "|") <$> in08
   in count ((`elem` [2, 3, 4, 7]) . T.length) $ fold outputvals

data Segment
  = A -- top
  | B -- top left
  | C -- top right
  | D -- middle
  | E -- bottom left
  | F -- bottom right
  | G -- bottom
  deriving (Eq, Ord, Enum, Bounded, Show)

segments :: Natural -> [Segment]
segments n
  | n > 9 = error "must give segments a digit"
  | otherwise = case n of
    0 -> [A, B, C, E, F, G]
    1 -> [C, F]
    2 -> [A, C, D, E, G]
    3 -> [A, C, D, F, G]
    4 -> [B, C, D, F]
    5 -> [A, B, D, F, G]
    6 -> [A, B, D, E, F, G]
    7 -> [A, C, F]
    8 -> [A .. G]
    _ -> [A, B, C, D, F, G]

determine :: [Text] -> Map Char Segment
determine ts =
  let m :: Mop Int [String] = foldMap one [(T.length t, [sort $ toString t]) | t <- ts]
      uno = maybe [] (foldMap toString) (m !? 2)
      siete = maybe [] (foldMap toString) (m !? 3)
      quattro = maybe [] (foldMap toString) (m !? 4)
      [top] = siete \\ uno
      [bottom] =
        nubOrd . fold . filter ((1 ==) . length) $
          (m !? 5 ?: []) <&> (\\ (top : quattro))
      [middle] =
        nubOrd . fold . filter ((1 ==) . length) $
          (m !? 5 ?: []) <&> (\\ (bottom : siete))
      [topleft] = quattro \\ (middle : uno)
      [bottomleft] =
        nubOrd . fold . filter ((1 ==) . length) $
          (m !? 6 ?: []) <&> (\\ (top : bottom : quattro))
      bottomright =
        let five = filter (elem topleft . (\\ (bottom : middle : siete)) . toString) (m !? 5 ?: [])
         in withNonEmpty (error "no bottom-right") head $ foldMap toString five \\ [top, middle, bottom, topleft]
      topright =
        withNonEmpty (error "no top-right") head $
          maybe [] (foldMap toString) (m !? 7) \\ [top, middle, bottom, topleft, bottomleft, bottomright]
   in relist
        [ (top, A),
          (topleft, B),
          (topright, C),
          (middle, D),
          (bottomleft, E),
          (bottomright, F),
          (bottom, G)
        ]

displaying :: Map Char Segment -> Text -> Natural
displaying m (toString -> cs) =
  let sgmts = sort $ mapMaybe (m !?) cs
   in find ((sgmts ==) . segments) [0 .. 9] ?: error "invalid segments"

display :: [Text] -> Natural
display (splitOn (pure "|") -> [det, out]) =
  let m = determine det
      outs = map (displaying m) out
   in undigits $ relist outs
display _ = error "invalid"

part2 :: Natural
part2 = sumOn display in08
