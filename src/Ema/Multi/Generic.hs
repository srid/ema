{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Multi.Generic where

import Data.SOP (I (..), NP (..))
import Data.SOP.Extra (NPConst (npConstFrom))
import Ema.Multi
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder
import Optics.Core (iso)

{- |
type family RCode (xss :: [[Type]]) :: [Type] where
  RCode '[] = '[]
  RCode ('[] ': rest) = Void ': RCode rest
  RCode ('[x] ': rest) = x ': RCode rest
  RCode (_ ': rest) = TypeError ( 'Text "MultiRoute: too many arguments")
-}

{- | MotleyRoute is a class of routes with an underlying MultiRoute (and MultiModel) representation.

 The idea is that by deriving MotleyRoute (and MotleyModel), we get IsRoute for free (based on MultiRoute).

 TODO: Rename this class, or change the API.
-}
class MotleyRoute r where
  -- | The model associated with `r`
  type MotleyRouteSubRoutes r :: [Type]

  -- TODO: These should instead be an Iso
  toMultiR :: r -> MultiRoute (MotleyRouteSubRoutes r)
  fromMultiR :: MultiRoute (MotleyRouteSubRoutes r) -> r

class MotleyRoute r => MotleyModel r where
  type MotleyModelType r :: Type
  toMultiM :: MotleyModelType r -> NP I (MultiModel (MotleyRouteSubRoutes r))
  _fromMultiM :: NP I (MultiModel (MotleyRouteSubRoutes r)) -> MotleyModelType r

-- | Mark a route as associated with a model type.
newtype WithModel r a = WithModel r

instance MotleyRoute r => MotleyRoute (WithModel r a) where
  type MotleyRouteSubRoutes (WithModel r a) = MotleyRouteSubRoutes r
  toMultiR (WithModel r) = toMultiR @r r
  fromMultiR = WithModel . fromMultiR @r

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
      & mapRouteEncoderRoute (iso fromMultiR toMultiR)
      & mapRouteEncoderModel (toMultiM @r)
  allRoutes m =
    WithModel . fromMultiR
      <$> allRoutes (toMultiM @r m)

-- | Like `WithModel`, but all sub-routes (at any depth) have `a` as their model.
newtype WithConstModel r (a :: Type) = WithConstModel r

instance MotleyRoute r => MotleyRoute (WithConstModel r a) where
  type MotleyRouteSubRoutes (WithConstModel r a) = MotleyRouteSubRoutes r
  toMultiR (WithConstModel r) = toMultiR @r r
  fromMultiR = WithConstModel . fromMultiR @r

-- Enables derivingVia of MotleyModel
instance
  ( MotleyRoute r
  , NPConst I (MultiModel (MotleyRouteSubRoutes r)) a
  ) =>
  MotleyModel (WithConstModel r a)
  where
  type MotleyModelType (WithConstModel r a) = a
  toMultiM = npConstFrom . I
  _fromMultiM = undefined -- _fromMultiM @r

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
      & mapRouteEncoderRoute (iso fromMultiR toMultiR)
      & mapRouteEncoderModel (toMultiM @r)
  allRoutes m =
    WithConstModel . fromMultiR
      <$> allRoutes (toMultiM @r m)
