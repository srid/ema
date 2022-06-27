{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Multi.Generic where

import Data.SOP.Extra (NPConst (npConstFrom))
import Ema.Multi (MultiModel, MultiRoute)
import Ema.Multi.Generic.Motley (
  HasSubModels (..),
  HasSubRoutes (..),
 )
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder (
  mapRouteEncoderModel,
  mapRouteEncoderRoute,
 )
import Generics.SOP (I (..), NP)
import Optics.Core (
  ReversibleOptic (re),
  coercedTo,
  review,
  (%),
 )
import Prelude hiding (All, Generic)

-- | Mark a route as associated with a model type.
newtype WithModel r a = WithModel r

instance HasSubRoutes r => HasSubRoutes (WithModel r a) where
  type SubRoutes (WithModel r a) = SubRoutes r
  subRoutesIso =
    coercedTo % subRoutesIso @r % coercedTo

instance
  ( HasSubRoutes r
  , HasSubModels r
  , mr ~ MultiRoute (SubRoutes r)
  , mm ~ MultiModel (SubRoutes r)
  , a ~ SubModels r
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
  subRoutesIso =
    coercedTo % subRoutesIso @r % coercedTo

-- Enables derivingVia of HasSubModels
instance
  ( HasSubRoutes r
  , NPConst I (MultiModel (SubRoutes r)) a
  ) =>
  HasSubModels (WithConstModel r a)
  where
  type SubModels (WithConstModel r a) = a
  subModels = npConstFrom . I

instance
  ( HasSubRoutes r
  , HasSubModels r
  , mr ~ MultiRoute (SubRoutes r)
  , mm ~ MultiModel (SubRoutes r)
  , NPConst I mm a -- The only difference
  , a ~ SubModels r
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
