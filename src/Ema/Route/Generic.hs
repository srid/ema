{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Generic (
  WithModel (WithModel),
  module X,
) where

import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder (mapRouteEncoder)
import Ema.Route.Generic.Sub as X
import Ema.Route.Lib.Multi (MultiModel, MultiRoute)
import GHC.TypeLits (
  ErrorMessage (ShowType, Text, (:<>:)),
 )
import GHC.TypeLits.Extra (Impossible (impossible), TypeErr)
import Generics.SOP (I (..), NP (Nil, (:*)))
import Optics.Core (ReversibleOptic (re), equality, review)
import Prelude hiding (All, Generic)

-- | Mark a route as associated with a model type.
newtype WithModel r a = WithModel r
  deriving newtype (HasSubRoutes)

instance
  ( HasSubRoutes r
  , HasSubModels r
  , mr ~ MultiRoute (SubRoutes r)
  , mm ~ MultiModel (SubRoutes r)
  , a ~ RouteModel r
  , IsRoute mr
  , RouteModel mr ~ NP I mm
  ) =>
  IsRoute (WithModel r a)
  where
  type RouteModel (WithModel r a) = a
  routeEncoder =
    routeEncoder @mr
      & mapRouteEncoder equality (re subRoutesIso) (subModels @r)
  allRoutes m =
    WithModel . review subRoutesIso
      <$> allRoutes (subModels @r m)

-- Enables derivingVia of HasSubModels
instance
  ( HasSubRoutes r
  , NPConst r I (MultiModel (SubRoutes r)) a
  ) =>
  HasSubModels (WithModel r a)
  where
  subModels = npConstFrom (Proxy @r) . I

{- | Like `NP` but all elements are the same.

 Each of `xs` is equivalent to `a`.
-}
class NPConst t (f :: k -> Type) (xs :: [k]) (a :: k) where
  -- | Create a `NP` given the constant element.
  npConstFrom :: Proxy t -> f a -> NP f xs

instance NPConst t f '[] a where
  npConstFrom _ _ = Nil

instance {-# OVERLAPPING #-} (NPConst t f xs (), f ~ I) => NPConst t f (() ': xs) () where
  npConstFrom p x = I () :* npConstFrom @_ @t @f @xs @() p x

instance {-# OVERLAPPING #-} (NPConst t f xs x, f ~ I) => NPConst t f (() ': xs) x where
  npConstFrom p x = I () :* npConstFrom @_ @t @f @xs @x p x

instance {-# OVERLAPPABLE #-} (NPConst t f xs x) => NPConst t f (x ': xs) x where
  npConstFrom p x = x :* npConstFrom @_ @t @f @xs @x p x

instance
  {-# OVERLAPPABLE #-}
  ( TypeErr
      ( 'Text "You must *manually* derive HasSubModels for type '"
          ':<>: 'ShowType t
          ':<>: 'Text "', because it can only be anyclass-derived if all sub-models are of type '"
          ':<>: 'ShowType x
          ':<>: 'Text "'"
      )
  ) =>
  NPConst t f xs x
  where
  npConstFrom = impossible
