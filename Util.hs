module Util where

import Control.Applicative
import Control.Category
import Control.Category.Unicode
import Control.Monad
import Data.Bool
import Data.Foldable
import Data.Function (($), flip)
import Data.Maybe
import Data.Traversable
import Numeric.Algebra
import Numeric.Partial.Group

commuteWith :: (Group b) => (a -> a -> b) -> a -> a -> b
commuteWith f x y = f x y - f y x

monus :: (Monoidal a, PartialGroup a) => a -> a -> a
monus x y = fromMaybe zero (pminus x y)

any2 :: (Applicative p, Foldable p) => (a -> b -> Bool) -> p a -> p b -> Bool
any2 f xs ys = any id $ liftA2 f xs ys

traverse2 :: (Traversable t, Applicative t, Applicative p) =>
    (a -> b -> p c) -> t a -> t b -> p (t c)
traverse2 f xs ys = sequenceA (liftA2 f xs ys)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f m_x m_y = m_x >>= \ x -> m_y >>= \ y -> f x y

infixr 9 &
(&) :: Category cat => cat a b -> cat b c -> cat a c
(&) = flip (∘)
