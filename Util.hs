module Util where

import Control.Applicative
import Control.Category
import Control.Category.Unicode
import Data.Function (flip)
import Data.Traversable

traverse2 :: (Traversable t, Applicative t, Applicative p) =>
    (a -> b -> p c) -> t a -> t b -> p (t c)
traverse2 f xs ys = sequenceA (liftA2 f xs ys)

infixr 9 &
(&) :: Category cat => cat a b -> cat b c -> cat a c
(&) = flip (∘)
