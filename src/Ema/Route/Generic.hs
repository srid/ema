{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Generic (
  WithModel (WithModel),
  module X,
) where

import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder.Type (mapRouteEncoder)
import Ema.Route.Generic.Sub as X
import Ema.Route.Lib.Multi (MultiModel, MultiRoute)
import Generics.SOP (I (..), NP)
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
  , RouteModel r ~ RouteModel (WithModel r a)
  , GSubModels (RouteModel r) (MultiModel (SubRoutes r)) (MultiModel (SubRoutes r))
  ) =>
  HasSubModels (WithModel r a)
  where
  subModels =
    subModels @(r `WithSubModels` MultiModel (SubRoutes r))
