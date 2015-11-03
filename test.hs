{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveFunctor, DeriveTraversable, UndecidableInstances #-}

import Prelude (IO, Integer, Show, toEnum, fromEnum, enumFromTo)
import Control.Applicative
import Control.Category
import Control.Category.Unicode
import Control.Monad
import Control.Monad.Trans.Class
import Data.Bool
import Data.CList
import Data.Eq
import Data.Foldable hiding (sum)
import Data.Function (($), flip)
import qualified Data.List as List
import Data.Monoid
import Data.Ord
import Data.Traversable
import Numeric.Algebra
import Numeric.Decidable.Zero
import Numeric.Domain.Euclidean
import Numeric.Field.Fraction
import Numeric.Partial.Group
import Numeric.Partial.Monoid
import Numeric.Partial.Semigroup
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck
import Util

import Data.Polynom.Gröbner
import Data.Polynom.Impl
import Data.Polynom.Show ()

instance (Serial m α, Serial m a, Ord α, Abelian a, DecidableZero a) => Serial m (Polynom α a) where
    series = series >>- \ (ListSet αs) ->
             fromList ∘ List.zip αs <$>
             mapS id (series <$ αs) `suchThat` foldl (pure (not ∘ isZero)) True
      where mapS f = List.foldr (\ x ys -> (:) <$> f x <~> ys) (pure [])

suchThat :: Monad m => Series m a -> (a -> Bool) -> Series m a
suchThat s p = s >>= liftA2 (bool empty) pure p

newtype ListSet a = ListSet [a] deriving (Show)
instance (Serial m a) => Serial m (ListSet a) where
    series = getDepth >>= lift ∘ flip listM series >>=
             foldr ((<|>) ∘ pure ∘ ListSet) empty ∘ List.inits

instance Monad m => Serial m Natural where
    series = generate $ flip List.take [0..]

instance (Monad m) => Serial m (Ratio Integer) where
    series = (,) <$> series <~> series >>= \ (m, Positive n) -> case gcd m n of
        1 -> pure (m%n)
        _ -> empty

newtype Monom n a = Monom (CList n a) deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

deriving instance Applicative (CList n) => Applicative (Monom n)

instance Monad m => Serial m (Monom Zero Natural) where
    series = pure (Monom Nil)

instance (Monad m, Serial m (Monom n Natural)) => Serial m (Monom (Succ n) Natural) where
    series = enumFromTo 0 ∘ toEnum <$> getDepth >>-
             foldr (\/) empty ∘ fmap (\ k ->
                                      (\ (Monom ks) -> Monom (k:.ks)) <$>
                                      localDepth (+ negate (fromEnum k)) series)

instance (Applicative (CList n), Monoidal a) => Monoid (Monom n a) where
    mempty = Monom (pure zero)
    Monom αs `mappend` Monom βs = Monom (liftA2 (+) αs βs)

instance (Applicative (CList n), Additive a) => Additive (Monom n a) where
    Monom αs + Monom βs = Monom (liftA2 (+) αs βs)

instance (Applicative (CList n), PartialSemigroup a) => PartialSemigroup (Monom n a) where
    Monom αs `padd` Monom βs = Monom <$> traverse2 padd αs βs

instance (Applicative (CList n), PartialMonoid a) => PartialMonoid (Monom n a) where
    pzero = Monom (pure pzero)

instance (Applicative (CList n), PartialGroup a) => PartialGroup (Monom n a) where
    Monom αs `pminus` Monom βs = Monom <$> traverse2 pminus αs βs

main :: IO ()
main = defaultMain $
    testGroup "root"
    [testProperty "Distributivity" $ \ p q r ->
     isZero (p*r + q*r - (p + q)*r :: P2 Integer),
     testGroup "Differentiation"
     [testProperty "Linearity" $ \ α p q ->
      isZero (formalDiff α p + formalDiff α q - formalDiff α (p + q) :: P2 Integer),
      testProperty "Product Rule" $ \ p q ->
      flip all (Monom <$> [1:.0:.Nil, 0:.1:.Nil]) $ \ α ->
      isZero (formalDiff α (p*q) - formalDiff α p*q - p*formalDiff α q :: P2 Integer)],
     testProperty "Gröbner" $ gröbner & \ ps ->
     all isZero [reduce ps (spol p q) :: P2 (Fraction Integer) | p <- ps, q <- ps]]

type P2 = Polynom (Monom (Succ (Succ Zero)) Natural)
