{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Generic (
  GenericRoute (GenericRoute),
  Opt (..),
  module X,
) where

import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder.Type (mapRouteEncoder)
import Ema.Route.Generic.RGeneric
import Ema.Route.Generic.SubModel as X
import Ema.Route.Generic.SubRoute as X
import Ema.Route.Lib.Multi (MultiModel, MultiRoute)
import GHC.Generics qualified as GHC
import Generics.SOP (I (..), NP)
import Optics.Core (ReversibleOptic (re), equality, review)
import Prelude hiding (All, Generic)

newtype GenericRoute r (opts :: [Opt]) = GenericRoute r
  deriving stock (GHC.Generic)

data Opt
  = RWithModel Type
  | RWithSubRoutes [Type]
  | RWithSubModels [Type]

type family OptModel (opts :: [Opt]) :: Type where
  OptModel '[] = ()
  OptModel ( 'RWithModel t ': _) = t
  OptModel ( 'RWithSubRoutes _ ': opts) = OptModel opts
  OptModel ( 'RWithSubModels _ ': opts) = OptModel opts

type family OptSubRoutes r (opts :: [Opt]) :: [Type] where
  OptSubRoutes r '[] = GSubRoutes (RDatatypeName r) (RConstructorNames r) (RCode r)
  OptSubRoutes _ ( 'RWithSubRoutes ts ': _) = ts
  OptSubRoutes r ( 'RWithSubModels _ ': opts) = OptSubRoutes r opts
  OptSubRoutes r ( 'RWithModel _ ': opts) = OptSubRoutes r opts

type family OptSubModels r (opts :: [Opt]) :: [Type] where
  OptSubModels r '[] = MultiModel (SubRoutes r)
  OptSubModels _ ( 'RWithSubModels ts ': opts) = ts
  OptSubModels r ( 'RWithSubRoutes _ ': opts) = OptSubModels r opts
  OptSubModels r ( 'RWithModel _ ': opts) = OptSubModels r opts

instance (RGeneric r, ValidSubRoutes r (OptSubRoutes r opts)) => HasSubRoutes (GenericRoute r opts) where
  type SubRoutes (GenericRoute r opts) = OptSubRoutes r opts
  subRoutesIso' =
    (,)
      (gtoSubRoutes @r @(OptSubRoutes r opts) . rfrom . coerce @_ @r)
      (coerce @r . rto . gfromSubRoutes @r)

instance
  ( GSubModels (RouteModel (GenericRoute r opts)) (MultiModel (OptSubRoutes r opts)) (OptSubModels r opts)
  , HasSubRoutes (GenericRoute r opts)
  ) =>
  HasSubModels (GenericRoute r opts)
  where
  subModels m =
    gsubModels @_ @(RouteModel (GenericRoute r opts))
      @(MultiModel (SubRoutes (GenericRoute r opts)))
      @(OptSubModels r opts)
      m

instance
  ( HasSubRoutes r
  , HasSubModels r
  , ValidSubRoutes r (SubRoutes r)
  , RGeneric r
  , mr ~ MultiRoute (SubRoutes r)
  , mm ~ MultiModel (SubRoutes r)
  , a ~ RouteModel r
  , a ~ OptModel opts
  , SubRoutes r ~ OptSubRoutes r opts
  , IsRoute mr
  , RouteModel mr ~ NP I mm
  ) =>
  IsRoute (GenericRoute r opts)
  where
  type RouteModel (GenericRoute r opts) = OptModel opts
  routeEncoder =
    routeEncoder @mr
      & mapRouteEncoder equality (re subRoutesIso) (subModels @r)
  allRoutes m =
    GenericRoute . review subRoutesIso
      <$> allRoutes (subModels @r m)