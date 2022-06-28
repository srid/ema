{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Route.Generic (
  -- DerivingVia utilities
  WithModel (WithModel),
  WithConstModel (WithConstModel),
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
import Generics.SOP (I (..), NP, IsProductType, productTypeFrom)
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
  , IsProductType a (MultiModel (SubRoutes r))
  ) =>
  HasSubModels (WithModel r a)
  where
  subModels = productTypeFrom

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

-- | Like `WithModel`, but all (immediate) sub-routes also have `a` as their model.
newtype WithConstModel r (a :: Type) = WithConstModel r

instance HasSubRoutes r => HasSubRoutes (WithConstModel r a) where
  type SubRoutes (WithConstModel r a) = SubRoutes r
  subRoutesIso' =
    bimap (lmap coerce) (rmap coerce) $ subRoutesIso' @r

-- Enables derivingVia of HasSubModels
instance
  ( HasSubRoutes r
  , NPConst I (MultiModel (SubRoutes r)) a
  ) =>
  HasSubModels (WithConstModel r a)
  where
  subModels = npConstFrom . I

instance
  ( HasSubRoutes r
  , HasSubModels r
  , mr ~ MultiRoute (SubRoutes r)
  , mm ~ MultiModel (SubRoutes r)
  , NPConst I mm a -- The only difference
  , a ~ RouteModel r
  , IsRoute mr
  , RouteModel mr ~ NP I mm
  ) =>
  IsRoute (WithConstModel r a)
  where
  type RouteModel (WithConstModel r a) = a
  routeEncoder =
    routeEncoder @mr
      & mapRouteEncoderRoute (re subRoutesIso)
      & mapRouteEncoderModel (subModels @r)
  allRoutes m =
    WithConstModel . review subRoutesIso
      <$> allRoutes (subModels @r m)
