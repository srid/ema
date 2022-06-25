{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Multi.Generic where

import Data.SOP.Constraint (AllZipF)
import Data.SOP.Extra (NPConst (npConstFrom))
import Data.SOP.NS (trans_NS)
import Ema.Multi (MultiModel, MultiRoute)
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder (
  mapRouteEncoderModel,
  mapRouteEncoderRoute,
 )
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Generics.SOP
import Optics.Core (
  Iso',
  ReversibleOptic (re),
  coercedTo,
  iso,
  review,
  (%),
 )
import Prelude hiding (All, Generic)

{- | MotleyRoute is a class of routes with an underlying MultiRoute (and MultiModel) representation.

 The idea is that by deriving MotleyRoute (and MotleyModel), we get IsRoute for free (based on MultiRoute).

 TODO: Rename this class, or change the API.
-}
class MotleyRoute r where
  -- | The sub-routes in the `r` (for each constructor).
  type MotleyRouteSubRoutes r :: [Type] -- TODO: Derive this generically

  motleyRouteIso :: Iso' r (MultiRoute (MotleyRouteSubRoutes r))
  default motleyRouteIso ::
    ( RGeneric r
    , SameShapeAs (RCode r) (MotleyRouteSubRoutes r)
    , SameShapeAs (MotleyRouteSubRoutes r) (RCode r)
    , All Top (RCode r)
    , All Top (MotleyRouteSubRoutes r)
    , AllZipF Coercible (RCode r) (MotleyRouteSubRoutes r)
    , AllZipF Coercible (MotleyRouteSubRoutes r) (RCode r)
    ) =>
    Iso' r (MultiRoute (MotleyRouteSubRoutes r))
  motleyRouteIso =
    iso (gtoMotley @r . rfrom) (rto . gfromMotley @r)

gtoMotley ::
  forall r.
  ( RGeneric r
  , SameShapeAs (RCode r) (MotleyRouteSubRoutes r)
  , SameShapeAs (MotleyRouteSubRoutes r) (RCode r)
  , All Top (RCode r)
  , All Top (MotleyRouteSubRoutes r)
  , AllZipF Coercible (RCode r) (MotleyRouteSubRoutes r)
  ) =>
  NS I (RCode r) ->
  MultiRoute (MotleyRouteSubRoutes r)
gtoMotley = trans_NS (Proxy @Coercible) coerce

gfromMotley ::
  forall r.
  ( RGeneric r
  , SameShapeAs (RCode r) (MotleyRouteSubRoutes r)
  , SameShapeAs (MotleyRouteSubRoutes r) (RCode r)
  , All Top (RCode r)
  , All Top (MotleyRouteSubRoutes r)
  , AllZipF Coercible (MotleyRouteSubRoutes r) (RCode r)
  ) =>
  MultiRoute (MotleyRouteSubRoutes r) ->
  NS I (RCode r)
gfromMotley = trans_NS (Proxy @Coercible) coerce

class MotleyRoute r => MotleyModel r where
  type MotleyModelType r :: Type

  -- | Break the model into a list of sub-models used correspondingly by the sub-routes.
  motleySubModels :: MotleyModelType r -> NP I (MultiModel (MotleyRouteSubRoutes r))

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

-- | Like `Generic` but for Route types only.
class Generic r => RGeneric r where
  type RCode r :: [Type]
  rfrom :: r -> NS I (RCode r)
  rto :: NS I (RCode r) -> r

instance (Generic r, RGeneric' (Code r), All RouteNP (Code r)) => RGeneric r where
  type RCode r = RCode' (Code r)
  rfrom = rfrom' @(Code r) . unSOP . from
  rto = to . SOP . rto' @(Code r)

class RGeneric' (xss :: [[Type]]) where
  type RCode' xss :: [Type]
  rfrom' :: NS (NP I) xss -> NS I (RCode' xss)
  rto' :: NS I (RCode' xss) -> NS (NP I) xss

instance RGeneric' '[] where
  type RCode' '[] = '[]
  rfrom' = \case {}
  rto' = \case {}

instance (RGeneric' xss, RouteNP xs) => RGeneric' (xs ': xss) where
  type RCode' (xs ': xss) = RouteNPType xs ': RCode' xss
  rfrom' = \case
    Z routeNP -> Z $ I $ fromRouteNP routeNP
    S rest -> S $ rfrom' @xss rest
  rto' = \case
    Z (I t) -> Z $ toRouteNP t
    S rest -> S $ rto' @xss rest

{- | Class of `NP` that can be used in a route tyupe.

    Ensures that the constructor can have 0 or 1 products only.
-}
class RouteNP (xs :: [Type]) where
  type RouteNPType xs :: Type
  fromRouteNP :: NP I xs -> RouteNPType xs
  toRouteNP :: RouteNPType xs -> NP I xs

instance RouteNP '[] where
  type RouteNPType '[] = ()
  fromRouteNP Nil = ()
  toRouteNP () = Nil

instance RouteNP (x ': '[]) where
  type RouteNPType (x ': '[]) = x
  fromRouteNP (I x :* Nil) = x
  toRouteNP x = I x :* Nil

instance (TypeError ( 'Text "MultiRoute: too many arguments")) => RouteNP (x ': x' ': xs) where
  type RouteNPType (x ': x' ': xs) = TypeError ( 'Text "MultiRoute: too many arguments")
  fromRouteNP _ = undefined
  toRouteNP _ = undefined
