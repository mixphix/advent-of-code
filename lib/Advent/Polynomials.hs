{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Advent.Polynomials where

import Data.Foldable.Toolbox (productOn)
import Data.Poly.Sparse.Semiring (pattern X)
import Data.Poly.Sparse.Semiring qualified as P
import Data.Semiring (Ring, zero)
import Data.Semiring qualified as S
import Data.Tuple.Toolbox ((***))
import Data.Vector.Unboxed qualified as V
import Relude.Extra.Map

type Polynomial = P.UPoly Int64

leading :: (Ring a) => Polynomial -> (a, Natural)
leading p = swap $ maybe (0, zero) (fromIntegral *** S.fromIntegral) (P.leading p)

scale :: Int64 -> Natural -> Polynomial -> Polynomial
scale r pow = P.scale (fromIntegral pow) (fromIntegral r)

-- |
-- The reciprocal polynomial is the polynomial whose coefficients
-- are in reverse order, __NOT__ @1/p(x)@.
-- See [reciprocal polynomial](https://en.wikipedia.org/wiki/Reciprocal_polynomial)
-- on Wikipedia
reciprocalPoly :: Polynomial -> Polynomial
reciprocalPoly = P.toPoly . V.reverse . P.unPoly

coeffsTo :: Natural -> Int64 -> Polynomial
coeffsTo _ 0 = zero
coeffsTo n a = sum [X ^ k | k <- [0, a .. fromIntegral n]]

partitionPieces :: [Natural] -> Natural -> Natural
partitionPieces xs n =
  let p = productOn (coeffsTo n . fromIntegral) xs
   in maybe 0 fromIntegral $ p !? fromIntegral n

instance StaticMap Polynomial where
  type Key _ = Int
  type Val _ = Int64
  size = fromIntegral . snd . leading @(Val Polynomial)
  lookup n = fmap snd . (V.!? n) . P.unPoly
  member k = V.elem k . V.map (fromIntegral . snd) . P.unPoly

findSmallest :: (Int64 -> Bool) -> Polynomial -> Maybe Natural
findSmallest f (V.reverse . P.unPoly -> v) = succ . fromIntegral <$> V.findIndex (f . snd) v
