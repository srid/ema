{-# LANGUAGE UndecidableInstances #-}

-- | Helpers for generics-sop
module Data.SOP.Extra (
  NPConst (npConstFrom),
) where

import GHC.TypeLits (
  ErrorMessage (ShowType, Text, (:$$:)),
  TypeError,
 )
import Generics.SOP (I (..), NP (..))

{- | Like `NP` but all elements are the same.

 Each of `xs` is equivalent to `a`.
-}
class NPConst (f :: k -> Type) (xs :: [k]) (a :: k) where
  -- | Create a `NP` given the constant element.
  npConstFrom :: f a -> NP f xs

instance NPConst f '[] a where
  npConstFrom _ = Nil

instance {-# OVERLAPPING #-} (NPConst f xs (), f ~ I) => NPConst f (() ': xs) () where
  npConstFrom x = I () :* npConstFrom @_ @f @xs @() x

instance {-# OVERLAPPING #-} (NPConst f xs x, f ~ I) => NPConst f (() ': xs) x where
  npConstFrom x = I () :* npConstFrom @_ @f @xs @x x

instance {-# OVERLAPPABLE #-} (NPConst f xs x) => NPConst f (x ': xs) x where
  npConstFrom x = x :* npConstFrom @_ @f @xs @x x

instance {-# OVERLAPPABLE #-} (TypeError ( 'Text "The NP type " ':$$: 'ShowType xs ':$$: 'Text " has elements distinct from " ':$$: 'ShowType x)) => NPConst f xs x where
  npConstFrom = undefined
