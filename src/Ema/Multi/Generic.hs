{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Multi.Generic where

import Data.SOP.Extra (NPConst (npConstFrom))
import Ema.Multi (MultiModel, MultiRoute)
import Ema.Multi.Generic.Motley
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder (
  mapRouteEncoderModel,
  mapRouteEncoderRoute,
 )
import Generics.SOP
import Optics.Core (
  ReversibleOptic (re),
  coercedTo,
  review,
  (%),
 )
import Prelude hiding (All, Generic)

-- | Mark a route as associated with a model type.
newtype WithModel r a = WithModel r

instance MotleyRoute r => MotleyRoute (WithModel r a) where
  type MotleyRouteSubRoutes (WithModel r a) = MotleyRouteSubRoutes r
  motleyRouteIso =
    coercedTo % motleyRouteIso @r % coercedTo

instance
  ( MotleyRoute r
  , MotleyModel r
  , mr ~ MultiRoute (MotleyRouteSubRoutes r)
  , mm ~ MultiModel (MotleyRouteSubRoutes r)
  , a ~ MotleyModelType r
  , IsRoute mr
  , RouteModel mr ~ NP I mm
  ) =>
  IsRoute (WithModel r a)
  where
  type RouteModel (WithModel r a) = a
  routeEncoder =
    routeEncoder @mr
      & mapRouteEncoderRoute (re motleyRouteIso)
      & mapRouteEncoderModel (motleySubModels @r)
  allRoutes m =
    WithModel . review motleyRouteIso
      <$> allRoutes (motleySubModels @r m)

-- | Like `WithModel`, but all sub-routes (at any depth) have `a` as their model.
newtype WithConstModel r (a :: Type) = WithConstModel r

instance MotleyRoute r => MotleyRoute (WithConstModel r a) where
  type MotleyRouteSubRoutes (WithConstModel r a) = MotleyRouteSubRoutes r
  motleyRouteIso =
    coercedTo % motleyRouteIso @r % coercedTo

-- Enables derivingVia of MotleyModel
instance
  ( MotleyRoute r
  , NPConst I (MultiModel (MotleyRouteSubRoutes r)) a
  ) =>
  MotleyModel (WithConstModel r a)
  where
  type MotleyModelType (WithConstModel r a) = a
  motleySubModels = npConstFrom . I

instance
  ( MotleyRoute r
  , MotleyModel r
  , mr ~ MultiRoute (MotleyRouteSubRoutes r)
  , mm ~ MultiModel (MotleyRouteSubRoutes r)
  , NPConst I mm a -- The only difference
  , a ~ MotleyModelType r
  , IsRoute mr
  , RouteModel mr ~ NP I mm
  ) =>
  IsRoute (WithConstModel r a)
  where
  type RouteModel (WithConstModel r a) = a
  routeEncoder =
    routeEncoder @mr
      & mapRouteEncoderRoute (re motleyRouteIso)
      & mapRouteEncoderModel (motleySubModels @r)
  allRoutes m =
    WithConstModel . review motleyRouteIso
      <$> allRoutes (motleySubModels @r m)
