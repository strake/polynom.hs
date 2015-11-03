module Data.Polynom.Gröbner where

import Control.Applicative
import Control.Category.Unicode
import Data.Bool
import Data.Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Traversable
import Numeric.Algebra hiding (Order (..))
import Numeric.Decidable.Zero
import Numeric.Partial.Group
import Util

import Data.Polynom.Impl

-- | Compute a Gröbner basis of the polynomial ideal with given generators.
--   Which Gröbner basis this computes is a function of the monomial order.
--   If we form a system of polynomial equations by equating all polynomials
--   of a given set to zero, such a system and its Gröbner basis have the same
--   set of solutions.
gröbner :: (Ord (p Natural), Monoid (p Natural),
            Applicative p, Traversable p,
            DecidableZero a, Division a, Semiring a, Group a) =>
           [Polynom (p Natural) a] -> [Polynom (p Natural) a]
gröbner ps = go ps [(p, q) | p <- ps, q <- ps, not (eq' p q), crit1 p q]
  where go gs [] = gs
        go gs ((p, q):bs)
          | isZero h = go gs bs
          | True     = go (h:gs) (bs List.++ [(h, g) | g <- gs, crit1 h g])
          where h = reduce gs (spol p q)

-- product criterion
crit1 :: (Applicative p, Foldable p, DecidableZero a) =>
         Polynom (p Natural) a -> Polynom (p Natural) a -> Bool
crit1 = any2 ((&&) `on` not ∘ isZero) `on` fromMaybe (pure 0) ∘ leadingMonom

reduce :: (Ord (p Natural), Monoid (p Natural),
           Applicative p, Traversable p,
           DecidableZero a, Semiring a, Group a, Division a) =>
          [Polynom (p Natural) a] -> Polynom (p Natural) a -> Polynom (p Natural) a
reduce qs = go
  where go p = case leadingMonom p of
            Nothing -> p
            Just α -> case [q | q <- qs, Just β <- [leadingMonom q],
                                         Just _ <- [traverse2 pminus α β]] of
                []    -> p
                (q:_) -> go (spol p q)

spol :: (Ord (p Natural), Semigroup (p Natural), Applicative p,
         Semiring a, Group a, DecidableZero a, Division a) =>
        Polynom (p Natural) a -> Polynom (p Natural) a -> Polynom (p Natural) a
spol p q = case (liftA2 (,) `on` leadingTerm) p q of
    Nothing -> zero
    Just ((α, a), (β, b)) -> b .* liftA2 monus β α `timesMonom` p - a .* liftA2 monus α β `timesMonom` q

eq' :: (DecidableZero a, Group a) => a -> a -> Bool
eq' x y = isZero (x - y)
