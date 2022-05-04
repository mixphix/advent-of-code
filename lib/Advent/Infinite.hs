module Advent.Infinite where

import Prelude hiding (splitAt)

infixr 5 :::

data Infinite a = a ::: Infinite a

-- | bogus; it's just the first list
chainInfinite :: Infinite a -> Infinite a -> Infinite a
chainInfinite (a ::: as) bs = a ::: chainInfinite as bs

instance Functor Infinite where
  fmap f (a ::: ia) = f a ::: fmap f ia

instance Applicative Infinite where
  pure a = a ::: pure a

  -- ziplist
  (f ::: fs) <*> (a ::: as) = f a ::: (fs <*> as)

instance Monad Infinite where
  (a ::: as) >>= f = f a `chainInfinite` (as >>= f)

repeating :: NonEmpty a -> Infinite a
repeating = join repeating'
 where
  repeating' :: NonEmpty a -> NonEmpty a -> Infinite a
  repeating' whole (a :| as) = a ::: repeating' whole (nonEmpty as ?: whole)

only :: Natural -> Infinite a -> [a]
only 0 _ = []
only n (a ::: as) = a : only (pred n) as

onlyWhile :: (a -> Bool) -> Infinite a -> [a]
onlyWhile f (a ::: as) = bool id (a :) (f a) $ onlyWhile f as

discard :: Natural -> Infinite a -> Infinite a
discard 0 as = as
discard n (_ ::: as) = discard (pred n) as

discardWhile :: (a -> Bool) -> Infinite a -> Infinite a
discardWhile f (a ::: as) = bool (a :::) (discardWhile f) (f a) as

infSplitAt :: Natural -> Infinite a -> ([a], Infinite a)
infSplitAt 0 as = ([], as)
infSplitAt n (a ::: as) = let (rest, bs) = infSplitAt n as in (a : rest, bs)

infChunksOf :: Natural -> Infinite a -> Infinite [a]
infChunksOf 0 _ = error "chunksOf 0"
infChunksOf n as = let (p, bs) = infSplitAt n as in p ::: infChunksOf n bs

(@@) :: Infinite a -> Natural -> a
(a ::: _) @@ 0 = a
(_ ::: as) @@ n = as @@ pred n

choices :: [a] -> [Infinite a]
choices as = (:::) <$> as <*> choices as
