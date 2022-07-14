module Day02 where

import Data.Text qualified as T

in02 :: [Text]
in02 = lines (input 2018 2)

charCounts :: Text -> Mop Int String
charCounts =
  withNonEmpty mempty (relist . map (length &&& one . head) . group1)
    . sort
    . toString

part1 :: Natural
part1 = app $ ((*) . count anyTwice &&& count anyThrice) in02
 where
  anyTwice, anyThrice :: Text -> Bool
  anyTwice t = isJust (charCounts t !? 2)
  anyThrice t = isJust (charCounts t !? 3)

oneDifferent :: Text -> Text -> Bool
oneDifferent t u = sort (uncurry (==) <$> T.zip t u) == False : replicate (T.length t - 1) True

part2 :: Text
part2 =
  [(t, u) | (t : ts) <- tails in02, u <- ts, oneDifferent t u]
  & (viaNonEmpty head >=> uncurry T.commonPrefixes)
    & \case
      Nothing -> ""
      Just (pre, postT, _) -> pre <> T.drop 1 postT
