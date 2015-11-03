module Data.Polynom.Impl where

import Control.Applicative
import Control.Category.Unicode
import Data.Bool
import Data.Foldable (Foldable (foldr), all)
import Data.Function (($), on)
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Traversable
import Data.Tuple
import Numeric.Algebra
import Numeric.Decidable.Zero
import Numeric.Domain.Euclidean hiding (degree)
import Numeric.Partial.Group
import Numeric.Semiring.Integral
import Util

-- | Polynomial in monomial exponents @α@ over coefficients @a@
newtype Polynom α a = Polynom (Map α a)

instance (Ord α, Monoidal a) => Additive (Polynom α a) where
    Polynom as + Polynom bs = Polynom (Map.unionWith (+) as bs)

instance (Ord α, Monoidal a, Abelian a) => Abelian (Polynom α a)

instance (Ord α, Monoidal a, Idempotent a) => Idempotent (Polynom α a)

instance {-# INCOHERENT #-} (Ord α, Monoidal a, Semiring a) => LeftModule a (Polynom α a) where
    n .* Polynom cs = Polynom ((n *) <$> cs)

instance {-# INCOHERENT #-} (Ord α, Monoidal a, Semiring a) => RightModule a (Polynom α a) where
    Polynom cs *. n = Polynom ((n *) <$> cs)

instance {-# INCOHERENT #-} (Ord α, Monoidal a, LeftModule b a) => LeftModule b (Polynom α a) where
    n .* Polynom cs = Polynom ((n .*) <$> cs)

instance {-# INCOHERENT #-} (Ord α, Monoidal a, RightModule b a) => RightModule b (Polynom α a) where
    Polynom cs *. n = Polynom ((*. n) <$> cs)

instance (Ord α, Monoidal a) => Monoidal (Polynom α a) where
    zero = Polynom Map.empty

instance (Ord α, Group a) => Group (Polynom α a) where
    negate (Polynom cs) = Polynom (negate <$> cs)

instance (Ord α, Semigroup α, Monoidal a, Semiring a) => Multiplicative (Polynom α a) where
    Polynom as * Polynom bs = fromList $
        (liftA2 (\ (α, a) (β, b) -> (α<>β, a*b)) `on` Map.assocs) as bs

instance (Ord α, Semigroup α, Abelian α, Monoidal a, Commutative a, Semiring a) => Commutative (Polynom α a)

instance (Ord α, Monoid α, Monoidal a, Unital a, Semiring a) => Unital (Polynom α a) where
    one = Polynom (Map.singleton mempty one)

instance (Ord α, DecidableZero a) => DecidableZero (Polynom α a) where
    isZero (Polynom cs) = all isZero cs

instance (Ord α, Semigroup α, Monoidal a, Semiring a) => Semiring (Polynom α a)

instance (Ord α, Semigroup α, Monoidal a, IntegralSemiring a) => IntegralSemiring (Polynom α a)

-- | Compute the /degree/ of a polynomial, the maximum total exponent of any monomial.
degree :: (Foldable p, Ord α, Monoidal α, DecidableZero a) => Polynom (p α) a -> Maybe α
degree (Polynom cs) = foldr (max ∘ Just ∘ sum) Nothing ((Map.keys ∘ Map.filter (not ∘ isZero)) cs)

-- | Compute the content of a polynomial over a unique factorization domain @a@.
--   The /content/ of such a polynomial is defined as the unit normal GCD of its coefficients.
content :: (Euclidean a) => Polynom α a -> a
content (Polynom cs) = gcd' (foldr (:) [] cs)

-- | Compute the primitive part of a polynomial over a unique factorization domain @a@.
--   A polynomial over a unique factorization domain is called /primitive/ if its coefficients
--   are all unit normal and pairwise coprime. The /primitive part/ of a polynomial @p@ is @q@
--   where @p = content p * q@.
primPart :: (Euclidean a) => Polynom α a -> Polynom α a
primPart p@(Polynom cs) = Polynom ((`quot` content p) <$> cs)

-- | Differentiate a polynomial. Each component of the given monomial exponent is how many times
--   to differentiate the polynomial by that variable.
formalDiff :: (Ord (p Natural), Applicative p, Traversable p, Abelian a, LeftModule Natural a) =>
    p Natural -> Polynom (p Natural) a -> Polynom (p Natural) a
formalDiff α (Polynom cs) = fromList [(γ, foldr (.*) b (liftA2 facQuot β γ))
                                        | (β, b) <- Map.assocs cs,
                                          Just γ <- [traverse2 pminus β α]]
  where facQuot n m = product [m+1..n]

leadingMonom :: DecidableZero a => Polynom α a -> Maybe α
leadingMonom = fmap fst ∘ leadingTerm

leadingTerm :: DecidableZero a => Polynom α a -> Maybe (α, a)
leadingTerm (Polynom cs) = fst <$> (Map.maxViewWithKey ∘ Map.filter (not ∘ isZero)) cs

leadingTerm' :: (Ord α, Monoid α, Abelian a, Monoidal a) => Polynom α a -> Polynom α a
leadingTerm' = fromList ∘ pure ∘ fromMaybe (mempty, zero) ∘ \ (Polynom cs) -> fst <$> Map.maxViewWithKey cs

timesMonom :: (Ord α, Semigroup α) => α -> Polynom α a -> Polynom α a
timesMonom α (Polynom as) = Polynom (Map.mapKeys (α <>) as)

fromList :: (Ord α, Abelian a) => [(α, a)] -> Polynom α a
fromList = Polynom ∘ Map.fromListWith (+)

type Semigroup = Monoid
