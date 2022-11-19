{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Generic (
  -- * Generic deriving types
  GenericRoute (GenericRoute),
  HasSubRoutes,
  HasSubModels,
  WithModel,
  WithSubRoutes,
  WithSubModels,

  -- * Customizing generic deriving
  GenericRouteOpt (..),

  -- * Handy functions
  subModels,

  -- * Export these for DerivingVia coercion representations
  FileRoute (FileRoute),
  FolderRoute (FolderRoute),
) where

import Ema.Route.Class (IsRoute (..))
import Ema.Route.Generic.RGeneric
import Ema.Route.Generic.SubModel as X
import Ema.Route.Generic.SubRoute as X
import Ema.Route.Generic.Verification
import Ema.Route.Lib.File (FileRoute (FileRoute))
import Ema.Route.Lib.Folder (FolderRoute (FolderRoute))
import Ema.Route.Lib.Multi (MultiModel, MultiRoute)
import Ema.Route.Prism.Type (mapRoutePrism)
import GHC.Generics qualified as GHC
import Generics.SOP (All, I (..), NP)
import Optics.Core (ReversibleOptic (re), coercedTo, equality, review, (%))
import Prelude hiding (All, Generic)

-- | DerivingVia type to generically derive `IsRoute`
newtype GenericRoute r (opts :: [Type]) = GenericRoute r
  deriving stock (GHC.Generic)

{- | Associate the route with the given model type.

 Default: `()`
-}
data WithModel (r :: Type)

{- | Specify isomorphic types to delegate sub-route behaviour. Usually this is identical to the route product type.

    The isomorphism is specified by @Coercible@.

    The default implementation uses @FileRoute@ for terminal routes, and
    @FolderRoute@ (with constructor prefix stripped) for wrapping sub-routes types.
-}
data WithSubRoutes (subRoutes :: [Type])

{- | Specify the @Data.Generics.Product.Any.HasAny@ selector type for sub models

  Note: if the selector is a @Symbol@ you must wrap it in a @Proxy@.
-}
data WithSubModels (subModels :: [Type])

{- | Typeclass to control `GenericRoute` behaviour.

  The `FooM` type enables users to define their type optionally, whose default
  is specified in the `Foo` type family (further below).

  You can define your own options, for example:

  @
    data MySubRoutes
    instance GenericRouteOpt r MySubRoutes where
      type
        OptSubRoutesM r MySubRoutes =
          'Just (GSubRoutes (RDatatypeName r) (RConstructorNames r) (RCode r))
  @

  And use it as:

  > deriving via (GenericRoute MyRoute '[MySubRoutes])
-}
class GenericRouteOpt (r :: Type) (opt :: Type) where
  type OptModelM r opt :: Maybe Type
  type OptModelM r opt = 'Nothing
  type OptSubRoutesM r opt :: Maybe [Type]
  type OptSubRoutesM r opt = 'Nothing
  type OptSubModelsM r opt :: Maybe [Type]
  type OptSubModelsM r opt = 'Nothing

instance GenericRouteOpt r (WithModel t) where
  type OptModelM r (WithModel t) = 'Just t
instance GenericRouteOpt r (WithSubRoutes t) where
  type OptSubRoutesM r (WithSubRoutes t) = 'Just t
instance GenericRouteOpt r (WithSubModels t) where
  type OptSubModelsM r (WithSubModels t) = 'Just t

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

instance
  ( VerifyModels
      (RouteModel (GenericRoute r opts))
      (MultiModel (SubRoutes (GenericRoute r opts)))
      (OptSubModels r opts)
  , VerifyRoutes (RCode r) (SubRoutes (GenericRoute r opts))
  , GSubModels (RouteModel (GenericRoute r opts)) (MultiModel (OptSubRoutes r opts)) (OptSubModels r opts)
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
  ( VerifyRoutes (RCode r) (SubRoutes (GenericRoute r opts))
  , HasSubRoutes r
  , HasSubModels r
  , ValidSubRoutes r (SubRoutes r)
  , RGeneric r
  , mr ~ MultiRoute (SubRoutes r)
  , mm ~ MultiModel (SubRoutes r)
  , RouteModel r ~ OptModel r opts
  , RouteModel mr ~ NP I mm
  , IsRoute mr
  , GenericRouteOpts r opts
  ) =>
  IsRoute (GenericRoute r opts)
  where
  type RouteModel (GenericRoute r opts) = OptModel r opts
  routePrism =
    routePrism @mr
      & mapRoutePrism equality (re (subRoutesIso @r) % coercedTo) (subModels @r)
  routeUniverse m =
    GenericRoute . review subRoutesIso
      <$> routeUniverse (subModels @r m)
