module Advent.Numbers where

import Advent.Functions (pairwise, palindrome, takeLNE, takeRNE)
import Advent.Orphans ()
import Control.Lens (Iso', iso, (^.), pattern Empty)
import Data.List.NonEmpty.Toolbox qualified as NE
import Data.List.Toolbox (allSame)
import Data.Ratio ((%))
import Data.Semiring (Add (..), Mul (..), Semiring)
import Data.Sequence.NonEmpty (NESeq, (|>), pattern (:||>))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Geometry.Point
import Geometry.Vector (Additive (..))
import Math.NumberTheory.ArithmeticFunctions qualified as NT
import Math.NumberTheory.Primes qualified as NT
import Math.NumberTheory.Primes.Counting qualified as NT
import Math.NumberTheory.Recurrences qualified as NT
import Relude
import Relude.Extra.Map (StaticMap (..))
import Relude.Extra.Newtype ((#.))
import Relude.Unsafe ((!!))

type Prime = NT.Prime Natural

type Ration = Ratio Natural

sumOn :: forall r t a. (Semiring r, Foldable t) => (a -> r) -> t a -> r
sumOn f = coerce #. foldMap' (coerce @_ @(Add r) . f)

productOn :: forall r t a. (Semiring r, Foldable t) => (a -> r) -> t a -> r
productOn f = coerce #. foldMap' (coerce @_ @(Mul r) . f)

primes :: [Prime]
primes = NT.primes

primely :: (Natural -> a) -> Prime -> a
primely = (. unPrime)

primelyF :: (Functor f) => (f Natural -> a) -> f Prime -> a
primelyF = (. fmap unPrime)

prime :: Natural -> Bool
prime = isJust . isPrime

isPrime :: Natural -> Maybe Prime
isPrime = NT.isPrime

unPrime :: Prime -> Natural
unPrime = NT.unPrime

nthPrime, precPrime, nextPrime :: Natural -> Prime
nthPrime = nextPrime . fromIntegral . NT.unPrime . NT.nthPrime . fromIntegral
precPrime = NT.precPrime
nextPrime = NT.nextPrime

factorise :: Natural -> NESet (Prime, Natural)
factorise = fromList . (second fromIntegral <<$>> NT.factorise)

unfactorise :: NESet (Prime, Natural) -> Natural
unfactorise = NT.factorBack . toList . NESet.map (second fromIntegral)

factors :: Iso' Natural (NESet (Prime, Natural))
factors = iso factorise unfactorise

type Base (n :: Nat) = Const (NESeq Natural) n

base :: forall n. (KnownNat n) => Natural -> Base n
base 0 = Const (pure 0)
base n = Const $ case natVal (Proxy @n) of
  0 -> error "based to base 0"
  b ->
    if n < b
      then pure n
      else
        let (q, r) = n `quotRem` b
            Const ds = base @n q
         in ds |> r

unbase :: forall n. (KnownNat n) => Base n -> Natural
unbase (Const (ds :||> d)) = case natVal (Proxy @n) of
  0 -> error "unbased from base 0"
  b -> d + b * NESeq.withNonEmpty 0 (unbase @n . Const) ds

rebase :: forall m n. (KnownNat m, KnownNat n) => Base m -> Base n
rebase = base @n . unbase @m

based :: forall n. (KnownNat n) => Iso' Natural (Base n)
based = iso base unbase

digits :: Natural -> NESeq Natural
digits = getConst . base @10

undigits :: NESeq Natural -> Natural
undigits = unbase @10 . Const

backward :: Natural -> Natural
backward = undigits . NESeq.reverse . digits

catdigits :: NonEmpty Natural -> Natural
catdigits = undigits . NE.foldMap1 digits

firstNDigits :: Natural -> Natural -> Maybe (NESeq Natural)
firstNDigits 0 _ = Nothing
firstNDigits n k = Just . takeLNE n $ digits k

lastNDigits :: Natural -> Natural -> Maybe (NESeq Natural)
lastNDigits 0 _ = Nothing
lastNDigits n k = Just . takeRNE n $ digits k

multipleOf :: (Integral a) => a -> a -> Bool
multipleOf k = (0 ==) . (`mod` k)

sq :: Natural -> Natural
sq = join (*)

repunit :: Natural -> Natural
repunit n = undigits $ NESeq.replicate (fromIntegral n) 1

floorsqrt :: Natural -> Natural
floorsqrt = floor @Double . sqrt . fromIntegral

numPrimesBelow :: Natural -> Natural
numPrimesBelow n = fromIntegral $ NT.primeCount (fromIntegral n)

ngon :: Integer -> Natural -> Natural
ngon s n = flip div 2 . fromIntegral $ fromIntegral (sq n) * (s - 2) - fromIntegral n * (s - 4)

ngonal :: Integer -> Natural -> Bool
ngonal (fromIntegral -> s) (fromIntegral -> x) =
  let n =
        (sqrt (8 * (s - 2) * x + join (*) (s - 4)) + (s - 4))
          / (2 * (s - 2)) ::
          Double
   in floor n == (ceiling n :: Int)

ngons :: Natural -> [Natural]
ngons s = go s 1 (s - 1)
 where
  go !s' !n !m = n : go s' (n + m) (m + s' - 2)

fibintseq :: (Integral n) => n -> n -> [n]
fibintseq !n0 !n1 = n0 : fibintseq n1 (n0 + n1)

fibonacci :: [Natural]
fibonacci = fibintseq 0 1

lucas :: [Natural]
lucas = fibintseq 2 1

divisors :: Natural -> Set Natural
divisors = NT.divisors

properDivisors :: Natural -> Set Natural
properDivisors = Set.delete <*> divisors

sumProperDivisors :: Natural -> Natural
sumProperDivisors = sum . properDivisors

numDivisors :: Natural -> Natural
numDivisors = NT.tau

totient :: Natural -> Natural
totient = NT.totient

numDistinctPrimeFactors :: Natural -> Natural
numDistinctPrimeFactors = NT.smallOmega

amicable :: Natural -> Bool
amicable n = n > 1 && n /= sumProperDivisors n && sumProperDivisors (sumProperDivisors n) == n

deficient :: Natural -> Bool
deficient n = n > 1 && n > sumProperDivisors n

perfect :: Natural -> Bool
perfect n = n > 1 && n == sumProperDivisors n

abundant :: Natural -> Bool
abundant n = n > 1 && n < sumProperDivisors n

squarefree :: Natural -> Bool
squarefree = NT.isNFree 2

partitions :: [Integer]
partitions = NT.partition

numPartitions :: Natural -> Natural
numPartitions n = fromIntegral $ partitions !! fromIntegral n

factorials :: [Natural]
factorials = NT.factorial

factorial :: Natural -> Natural
factorial n = factorials !! fromIntegral n

binomials :: Natural -> [Natural]
binomials = NT.binomialLine

choose :: Natural -> Natural -> Natural
choose n k = binomials n !! fromIntegral k

stirlings1 :: Natural -> [Natural]
stirlings1 n = NT.stirling1 !! fromIntegral n

stirling1 :: Natural -> Natural -> Natural
stirling1 n k = stirlings1 n !! fromIntegral k

stirlings2 :: Natural -> [Natural]
stirlings2 n = NT.stirling2 !! fromIntegral n

stirling2 :: Natural -> Natural -> Natural
stirling2 n k = stirlings2 n !! fromIntegral k

collatz :: Natural -> [Natural]
collatz 1 = [1]
collatz n | even n = n : collatz (n `div` 2)
collatz n = n : collatz (3 * n - 1)

hailstone :: Natural -> Natural
hailstone 1 = 1
hailstone !n = succ $! hailstone (if even n then n `div` 2 else 3 * n + 1)

narcissistic :: Natural -> Natural -> Bool
narcissistic k n = n == sum (digits n <&> (^ k))

ulampos :: Natural -> Point 2 Integer
ulampos 0 = Point2 0 0
ulampos n =
  let k = fromIntegral (floorsqrt n) :: Integer
      m = fromIntegral n - (k * k)
      d = m - fromIntegral (k + 1)
      half = flip quot 2 . fromIntegral
      f = fmap $ if even k then negate else id
      k2pos =
        if even k
          then Point2 (1 - half k) (half k)
          else Point2 (half $ fromIntegral k - 1) (half $ 1 - fromIntegral k)
      k2k1pos =
        if even k
          then Point2 (- half k) (- half k)
          else Point2 (half $ k + 1) (half $ k + 1)
   in if fromIntegral n == k * k
        then k2pos
        else
          if m <= succ k -- side length
            then Point . on (^+^) (^. vector) k2pos $ f (Point2 1 (m - 1))
            else Point . on (^-^) (^. vector) k2k1pos $ f (Point2 d 0)

unulampos :: Point 2 Integer -> Natural
unulampos (Point2 x y) =
  let q = abs x > abs y
      x' = fromIntegral $ on max abs x y
      y' = if q then x + y else x - y
      y'' = fromIntegral (abs y') + 4 * sq x' + 1
      x'' = x' * 2
   in if q
        then (if x > 0 then y'' - 2 * x'' else y'')
        else (if y > 0 then y'' - x'' else y'' + x'')

ulam :: Iso' Natural (Point 2 Integer)
ulam = iso ulampos unulampos

reciprocal :: Natural -> [Natural]
reciprocal = go 1
 where
  go n k = case n `divMod` k of
    (q, 0) -> [q]
    (digits -> (_ :||> lq), _) -> lq : go (10 * n) k

convergents :: [Natural] -> [Ration]
convergents [] = []
convergents [x] = [x % 1]
convergents (x : y : xs) = x % 1 : go (x * y + 1) y x 1 xs
 where
  go pn qn _ _ [] = [pn % qn]
  go pn qn pn1 qn1 (z : zs) = pn % qn : go (z * pn + pn1) (z * qn + qn1) pn qn zs

-- | Left = terminating
continuedFraction :: forall a. (Ord a, RealFrac a) => a -> Either [Natural] [Natural]
continuedFraction = cf Set.empty
 where
  cf :: (Ord a, RealFrac a) => Set a -> a -> Either [Natural] [Natural]
  cf recips (properFraction -> (i, f)) =
    bimap (i :) (i :) $
      if f == 0 || f `member` recips
        then Left []
        else cf (Set.insert f recips) (recip f)

arithmetic :: (Eq a, Num a) => [a] -> Bool
arithmetic = allSame . pairwise subtract

geometric :: (Eq a, Fractional a) => [a] -> Bool
geometric = allSame . pairwise (flip (/))

pandigital0 :: Natural -> Bool
pandigital0 k = let n = NESeq.sort (digits k) in n == digits 1234567890

pandigital1 :: Natural -> Bool
pandigital1 k = let n = NESeq.sort (digits k) in n == digits 123456789

pandigitals0 :: Natural -> [Natural]
pandigitals0 k | k > 9 = []
pandigitals0 k =
  undigits . NESeq.unsafeFromSeq . fromList
    <$> filter ((/= Just 0) . viaNonEmpty head) (permutations [0 .. k])

pandigitals1 :: Natural -> [Natural]
pandigitals1 k | k > 9 = []
pandigitals1 k = undigits . NESeq.unsafeFromSeq . fromList <$> permutations [1 .. k]

palindromic :: Natural -> Bool
palindromic = palindrome . toList . digits

palindromicIn :: forall n. (KnownNat n) => Const Natural n -> Bool
palindromicIn (Const n) = palindrome . toList . getConst $ base @n n

pythags :: [Point 3 Natural]
pythags =
  [ Point3 a b c
  | c <- [5 ..]
  , b <- [1 .. c]
  , a <- [1 .. b]
  , sq a + sq b == sq c
  ]

sums :: (Integral n) => Natural -> Set n -> Set n
sums 0 _ = Empty
sums k ns =
  let ks = sums (pred k) ns
   in ns <> foldMap (\n -> Set.map (n +) ks) ns
