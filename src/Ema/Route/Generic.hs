{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Generic (
  GenericRoute (GenericRoute),
  GenericRouteOpt (..),
  RWithModel,
  RWithSubRoutes,
  RWithSubModels,
  module X,
) where

import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder.Type (mapRouteEncoder)
import Ema.Route.Generic.RGeneric
import Ema.Route.Generic.SubModel as X
import Ema.Route.Generic.SubRoute as X
import Ema.Route.Lib.Multi (MultiModel, MultiRoute)
import GHC.Generics qualified as GHC
import Generics.SOP (All, I (..), NP)
import Optics.Core (ReversibleOptic (re), equality, review)
import Prelude hiding (All, Generic)

-- | DerivingVia type to generically derive `IsRoute`
newtype GenericRoute r (opts :: [Type]) = GenericRoute r
  deriving stock (GHC.Generic)

{- | Associate the route with the given model type.

 Default: `()`
-}
data RWithModel (r :: Type)

{- | Specify isomorphic types to delegate sub-route behaviour. Usually this is identical to the route product type.

    The isomorphism is specified by @GIsomorphic@ and is thus via generic representation.

    The default implementation uses @FileRoute@ for terminal routes, and
    @FolderRoute@ (with constructor prefix stripped) for wrapping sub-routes types.
-}
data RWithSubRoutes (subRoutes :: [Type])

{- | Specify the @Data.Generics.Product.Any.HasAny@ selector type for sub models

  Note: if the selector is a @Symbol@ you must wrap it in a @Proxy@.
-}
data RWithSubModels (subModels :: [Type])

-- | Typeclass to control `GenericRoute` behaviour.
class GenericRouteOpt (r :: Type) (opt :: Type) where
  type OptModelM r opt :: Maybe Type
  type OptSubRoutesM r opt :: Maybe [Type]
  type OptSubModelsM r opt :: Maybe [Type]

instance GenericRouteOpt r (RWithModel t) where
  type OptModelM r (RWithModel t) = 'Just t
  type OptSubRoutesM r (RWithModel t) = 'Nothing
  type OptSubModelsM r (RWithModel t) = 'Nothing
instance GenericRouteOpt r (RWithSubRoutes t) where
  type OptModelM r (RWithSubRoutes _) = 'Nothing
  type OptSubRoutesM r (RWithSubRoutes t) = 'Just t
  type OptSubModelsM r (RWithSubRoutes _) = 'Nothing
instance GenericRouteOpt r (RWithSubModels t) where
  type OptModelM r (RWithSubModels t) = 'Nothing
  type OptSubRoutesM r (RWithSubModels t) = 'Nothing
  type OptSubModelsM r (RWithSubModels t) = 'Just t

type family OptModel r (opts :: [Type]) :: Type where
  OptModel r '[] = ()
  OptModel r (opt ': opts) = FromMaybe (OptModel r opts) (OptModelM r opt)

type family OptSubRoutes r (opts :: [Type]) :: [Type] where
  OptSubRoutes r '[] = GSubRoutes (RDatatypeName r) (RConstructorNames r) (RCode r)
  OptSubRoutes r (opt ': opts) = FromMaybe (OptSubRoutes r opts) (OptSubRoutesM r opt)

type family OptSubModels r (opts :: [Type]) :: [Type] where
  OptSubModels r '[] = MultiModel (SubRoutes r)
  OptSubModels r (opt ': opts) = FromMaybe (OptSubModels r opts) (OptSubModelsM r opt)

type family FromMaybe (def :: a) (maybe :: Maybe a) :: a where
  FromMaybe def 'Nothing = def
  FromMaybe def ( 'Just a) = a

type GenericRouteOpts r opts = All (GenericRouteOpt r) opts

instance
  ( GenericRouteOpts r opts
  , RGeneric r
  , ValidSubRoutes r (OptSubRoutes r opts)
  ) =>
  HasSubRoutes (GenericRoute r opts)
  where
  type SubRoutes (GenericRoute r opts) = OptSubRoutes r opts
  subRoutesIso' =
    (,)
      (gtoSubRoutes @r @(OptSubRoutes r opts) . rfrom . coerce @_ @r)
      (coerce @r . rto . gfromSubRoutes @r)

instance
  ( GSubModels (RouteModel (GenericRoute r opts)) (MultiModel (OptSubRoutes r opts)) (OptSubModels r opts)
  , HasSubRoutes (GenericRoute r opts)
  , GenericRouteOpts r opts
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
  , a ~ OptModel r opts
  , SubRoutes r ~ OptSubRoutes r opts
  , IsRoute mr
  , RouteModel mr ~ NP I mm
  , GenericRouteOpts r opts
  ) =>
  IsRoute (GenericRoute r opts)
  where
  type RouteModel (GenericRoute r opts) = OptModel r opts
  routeEncoder =
    routeEncoder @mr
      & mapRouteEncoder equality (re subRoutesIso) (subModels @r)
  allRoutes m =
    GenericRoute . review subRoutesIso
      <$> allRoutes (subModels @r m)
