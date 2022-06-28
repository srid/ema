{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Route.Generic (
  -- DerivingVia utilities
  WithModel (WithModel),
) where

import Data.Profunctor
import Data.SOP.Extra (NPConst (npConstFrom))
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder (mapRouteEncoderModel, mapRouteEncoderRoute)
import Ema.Route.Generic.Sub (
  HasSubModels (..),
  HasSubRoutes (..),
  subRoutesIso,
 )
import Ema.Route.Lib.Multi (MultiModel, MultiRoute)
import Generics.SOP (I (..), NP)
import Optics.Core (ReversibleOptic (re), review)
import Prelude hiding (All, Generic)

-- | Mark a route as associated with a model type.
newtype WithModel r a = WithModel r

instance HasSubRoutes r => HasSubRoutes (WithModel r a) where
  type SubRoutes (WithModel r a) = SubRoutes r
  subRoutesIso' =
    bimap (lmap coerce) (rmap coerce) $ subRoutesIso' @r

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
      & mapRouteEncoderRoute (re subRoutesIso)
      & mapRouteEncoderModel (subModels @r)
  allRoutes m =
    WithModel . review subRoutesIso
      <$> allRoutes (subModels @r m)

-- Enables derivingVia of HasSubModels
instance
  ( HasSubRoutes r
  , NPConst I (MultiModel (SubRoutes r)) a
  ) =>
  HasSubModels (WithModel r a)
  where
  subModels = npConstFrom . I
