module Day07 where

import Advent
import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Base (TreeF (..))
import Data.Functor.Foldable (histo)
import Data.List.Toolbox (allSame, (\\))
import Data.Map.Strict qualified as Map
import Data.Tree (Tree (..), foldTree, levels, unfoldTree)

data Program = Program {name :: Text, weight :: Natural, aboves :: [Text]} deriving (Eq, Show)

program :: Parser Program
program = do
  name <- pname
  weight <- string " (" *> number <* string ")"
  aboves <- option [] $ string " -> " *> (pname `sepBy` string ", ")
  pure Program {..}
  where
    pname = toText <$> many letter

in07 :: (Map Text Natural, Map Text [Text])
weights :: Map Text Natural
relations :: Map Text [Text]
in07@(weights, relations) =
  foldMap (one . (name &&& weight) &&& one . (name &&& aboves))
    . mapMaybe (parsedWith program)
    $ lines (input 2017 7)

inverseRelations :: Map Text Text
inverseRelations = Map.foldMapWithKey (\k vs -> relist $ (,k) <$> vs) relations

part1 :: Text
part1 = withNonEmpty "" head $ keys relations \\ keys inverseRelations

tower :: Tree (Text, Natural)
tower = unfoldTree (\t -> ((t, weights !? t ?: 0), relations !? t ?: [])) part1

towerWeight :: Tree Natural -> Natural
towerWeight = foldTree (\w disc -> w + sum disc)

part2 :: Natural
part2 =
  let wss = relist @_ @(Map Natural Natural) $ withNonEmpty [] last (levels $ histo go $ snd <$> tower)
      Just sub = oddOneOut $ elems wss
      (w, tot) = Map.findMin $ Map.filter (sub ==) wss

      go :: TreeF Natural (Cofree (TreeF Natural) a) -> Tree (Natural, Natural)
      go (NodeF w0 (map g -> nns)) = Node (w0, w0) nns

      g :: Cofree (TreeF Natural) a -> Tree (Natural, Natural)
      g (_ :< NodeF w0 ns) =
        if allSame (snd <<$>> g <$> ns)
          then Node (w0, w0 + sumOn (towerWeight . fmap snd) (g <$> ns)) []
          else Node (w0, w0) (g <$> ns)
   in w - (tot - withNonEmpty 0 head (elems wss \\ [sub]))
