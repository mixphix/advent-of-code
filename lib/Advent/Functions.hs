module Advent.Functions where

import Control.Lens (Iso', iso, (^.), pattern Empty)
import Data.Char (toLower)
import Data.Containers.NonEmpty (withNonEmpty, pattern IsEmpty, pattern IsNonEmpty)
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Data.List.Toolbox (elemIndex)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Set qualified as Set
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import GHC.Exts (IsList (..))
import Linear (V2 (..))
import Relude.Extra.Map (DynamicMap (insert), StaticMap (member), (!?))

enum :: (Enum a, Enum b) => Iso' a b
enum = iso (toEnum . fromEnum) (toEnum . fromEnum)

listed :: (IsList f, IsList g, Item f ~ Item g) => Iso' f g
listed = iso (fromList . GHC.Exts.toList) (fromList . GHC.Exts.toList)

relist :: forall s t a b f g. (s a ~ f, t b ~ g, IsList f, IsList g, Item f ~ Item g) => f -> g
relist = (^. listed)

list2 :: a -> a -> [a]
list2 a b = [a, b]

infix 2 ?

(?) :: (Alternative f) => Bool -> a -> f a
b ? a = if b then pure a else empty

infixl 4 <<&>>

(<<&>>) :: (Functor f, Functor g) => g (f a) -> (a -> b) -> g (f b)
(<<&>>) = flip (<<$>>)

infixr 1 >-<

(>-<) :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
(>-<) = flip foldMap

padWith :: (Show a) => Int -> Char -> a -> String
padWith n c x = paddedWith n c (show x)

paddedWith :: Int -> Char -> String -> String
paddedWith n c x = replicate (n - length x) c <> x

count :: (Foldable f) => (a -> Bool) -> f a -> Natural
count p = foldl' (\acc x -> acc + p x ^. enum) 0

every :: a -> Bool
every = const True

countJust :: (Foldable f) => (a -> Maybe b) -> f a -> Natural
countJust f = count (isJust . f)

yeas, nays :: (Foldable f) => f Bool -> Natural
yeas = count id
nays = count not

hasMoreThan :: (Foldable f) => Natural -> (a -> Bool) -> f a -> Bool
hasMoreThan k p = (>= k) . count p

pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise f = zipWith f <*> drop 1

pairsTied :: [a] -> [NonEmpty a]
pairsTied ls = take (length ls) $ pairwise (\a b -> a :| one b) (cycle ls)

palindrome :: (Eq a) => [a] -> Bool
palindrome = (==) <*> reverse

uninterleave :: [a] -> ([a], [a])
uninterleave (x : y : zs) =
  let (xs, ys) = uninterleave zs
   in (x : xs, y : ys)
uninterleave xs = (xs, [])

gridToMap :: [[a]] -> Map (V2 Natural) a
gridToMap = go 0 0 Map.empty
  where
    go _ _ !m [] = m
    go r _ !m ([] : xss) = go (succ r) 0 m xss
    go r c !m ((x : xs) : xss) = go r (succ c) (insert (V2 c r) x m) (xs : xss)

mapToGrid :: Map (V2 Natural) a -> [[a]]
mapToGrid m =
  let (V2 c r, _) = Map.findMax m
   in [catMaybes [m !? V2 col row | col <- [0 .. c]] | row <- [0 .. r]]

-- | (0, 0) is the top-left corner
grid :: Iso' [[a]] (Map (V2 Natural) a)
grid = iso gridToMap mapToGrid

firstDuplicate :: forall a. (Ord a) => [a] -> Maybe a
firstDuplicate = go Empty
  where
    go :: Set a -> [a] -> Maybe a
    go _ [] = Nothing
    go seen (a : as)
      | a `member` seen = Just a
      | otherwise = go (Set.insert a seen) as

takeL :: Natural -> Seq a -> Seq a
takeL 0 _ = IsEmpty
takeL n (IsNonEmpty (a :<|| as)) = a :<| takeL (pred n) as
takeL _ _ = IsEmpty

takeLNE :: Natural -> NESeq a -> NESeq a
takeLNE 0 _ = error "takeLNE 0"
takeLNE n (a :<|| as) = a :<|| takeL (pred n) as

dropL :: Natural -> Seq a -> Seq a
dropL 0 as = as
dropL n (IsNonEmpty (_ :<|| as)) = dropL (pred n) as
dropL _ _ = IsEmpty

dropLNE :: Natural -> NESeq a -> Seq a
dropLNE 0 as = IsNonEmpty as
dropLNE n (_ :<|| as) = dropL (pred n) as

takeR :: Natural -> Seq a -> Seq a
takeR 0 _ = IsEmpty
takeR n (IsNonEmpty (as :||> a)) = takeR (pred n) as :|> a
takeR _ _ = IsEmpty

takeRNE :: Natural -> NESeq a -> NESeq a
takeRNE 0 _ = error "takeLNE 0"
takeRNE n (as :||> a) = takeR (pred n) as :||> a

dropR :: Natural -> Seq a -> Seq a
dropR 0 as = as
dropR n (IsNonEmpty (as :||> _)) = dropR (pred n) as
dropR _ _ = IsEmpty

dropRNE :: Natural -> NESeq a -> Seq a
dropRNE 0 as = IsNonEmpty as
dropRNE n (as :||> _) = dropR (pred n) as

slice :: (IsList f) => Natural -> Natural -> f -> f
slice m n (GHC.Exts.toList -> fa) = fromList . genericTake (n - m) $ genericDrop m fa

permutationsNE :: forall a. NonEmpty a -> NonEmpty (NonEmpty a)
permutationsNE lx = lx :| perms [] lx
  where
    perms :: [a] -> NonEmpty a -> [NonEmpty a]
    perms bs (a :| as) = foldl' interleave (withNonEmpty [] (perms (a : bs)) as) (permutations bs)
      where
        interleave :: [NonEmpty a] -> [a] -> [NonEmpty a]
        interleave rest = withNonEmpty rest (snd . interleaveWith id rest)

        interleaveWith :: (NonEmpty a -> NonEmpty a) -> [NonEmpty a] -> NonEmpty a -> ([a], [NonEmpty a])
        interleaveWith f rest (x :| xs) =
          let (us, vs) = withNonEmpty (as, rest) (interleaveWith (f . (x <|)) rest) xs
           in (x : us, f (a :| x : us) : vs)

oddOneOut :: (Ord a) => [a] -> Maybe a
oddOneOut xs | length xs < 3 = Nothing
oddOneOut xs = case NE.group (sort xs) of
  [a :| [], _] -> Just a
  [_, b :| []] -> Just b
  _ -> Nothing

timed :: (Show a) => a -> IO ()
timed x = do
  ts <- systemToUTCTime <$> getSystemTime
  putStr $ show x
  te <- systemToUTCTime <$> getSystemTime
  putStrLn $ " (" <> padWith 10 ' ' (diffUTCTime te ts) <> ")"

alphabetPos :: Char -> Natural
alphabetPos c = maybe (error "not a letter") fromIntegral $ toLower c `elemIndex` ['a' .. 'z']
