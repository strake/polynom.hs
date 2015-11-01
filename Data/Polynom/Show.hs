module Data.Polynom.Show () where

import Prelude hiding (Num (..))

import Data.Bool
import Data.Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import Numeric
import Numeric.Algebra hiding (Order (..))
import Numeric.Decidable.Zero

import Data.Polynom.Impl

instance (Foldable p, DecidableZero a, Show a) => Show (Polynom (p Natural) a) where
    show (Polynom cs) = List.intercalate " + " . toList . Map.mapWithKey showTerm .
                        Map.filter (not . isZero) $ cs

showTerm αs a =
    show a ++
    Prelude.fst
    (foldr (\ α (xs, n) ->
            (let v = bool [letters !! n] ("x" ++ showSub n)
                     (length αs > length letters)
             in xs ++ case α of 0 -> ""
                                1 -> v
                                _ -> v ++ showSuper (fromEnum α), n+1)) ("", 0) αs)

showSub = showWithNumerals ['₀'..'₉']
showSuper = showWithNumerals ['⁰','¹','²','³','⁴','⁵','⁶','⁷','⁸','⁹']
showWithNumerals ns = ($[]) . showIntAtBase (length ns) (ns !!)

letters = ['a'..'z'] ++ ['α'..'ω']
