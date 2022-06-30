{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ema.Route.Generic.Sub (
  HasSubRoutes (SubRoutes, subRoutesIso'),
  subRoutesIso,
  HasSubModels (subModels),
  -- DerivingVia types
  WithSubRoutes (WithSubRoutes),
  WithSubModels (WithSubModels),
  The (..),
  GSubModels (..),
  -- Export these for DerivingVia coercion representations
  FileRoute (FileRoute),
  FolderRoute (FolderRoute),
) where

import Data.SOP.Constraint (AllZipF)
import Data.SOP.NS (trans_NS)
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Route.Generic.RGeneric (RConstructorNames, RDatatypeName, RGeneric (..))
import Ema.Route.Lib.File (FileRoute (FileRoute))
import Ema.Route.Lib.Folder (FolderRoute (FolderRoute))
import Ema.Route.Lib.Multi (MultiModel, MultiRoute)
import GHC.TypeLits (AppendSymbol, Symbol)
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
import GHC.TypeLits.Extra.Symbol (StripPrefix, ToLower)
#else 
import GHC.TypeLits
#endif
import Data.Generics.Product (HasAny (the))
import Generics.SOP (
  All,
  I (..),
  NP (Nil, (:*)),
  NS,
  SameShapeAs,
  Top,
 )
import Generics.SOP.Type.Metadata qualified as SOPM
import Optics.Core (Iso', castOptic, equality, iso, united, view)
import Prelude hiding (All)

{- | HasSubRoutes is a class of routes with an underlying MultiRoute (and MultiModel) representation.

 The idea is that by deriving HasSubRoutes (and HasSubModels), we get IsRoute for free (based on MultiRoute).

 TODO: Rename this class, or change the API.
-}
class HasSubRoutes r where
  -- | The sub-routes in the `r` (for each constructor).
  type SubRoutes r :: [Type]

  type SubRoutes r = GSubRoutes (RDatatypeName r) (RConstructorNames r) (RCode r)

  -- You should use @subRoutesIso@ instead of this function directly.
  subRoutesIso' :: ((r -> MultiRoute (SubRoutes r)), (MultiRoute (SubRoutes r) -> r))
  default subRoutesIso' ::
    ( RGeneric r
    , ValidSubRoutes r (SubRoutes r)
    ) =>
    ((r -> MultiRoute (SubRoutes r)), (MultiRoute (SubRoutes r) -> r))
  subRoutesIso' =
    (,) (gtoSubRoutes @r . rfrom) (rto . gfromSubRoutes @r)

-- We cannot put this inside the `HasSubRoutes` type-class due to coercion issues with DerivingVia
-- See https://stackoverflow.com/a/71490273/55246
subRoutesIso :: HasSubRoutes r => Iso' r (MultiRoute (SubRoutes r))
subRoutesIso = uncurry iso subRoutesIso'

newtype r `WithSubRoutes` (subRoutes :: [Type]) = WithSubRoutes r
  deriving stock (Eq, Show)

instance
  ( RGeneric r
  , ValidSubRoutes r subRoutes
  ) =>
  HasSubRoutes (r `WithSubRoutes` subRoutes)
  where
  type SubRoutes (r `WithSubRoutes` subRoutes) = subRoutes
  subRoutesIso' =
    (,) (gtoSubRoutes @r @subRoutes . rfrom . coerce @_ @r) (coerce @r . rto . gfromSubRoutes @r)

gtoSubRoutes ::
  forall r subRoutes.
  ( RGeneric r
  , ValidSubRoutes r subRoutes
  ) =>
  NS I (RCode r) ->
  MultiRoute subRoutes
gtoSubRoutes = trans_NS (Proxy @Coercible) coerce

gfromSubRoutes ::
  forall r subRoutes.
  ( RGeneric r
  , ValidSubRoutes r subRoutes
  ) =>
  MultiRoute subRoutes ->
  NS I (RCode r)
gfromSubRoutes = trans_NS (Proxy @Coercible) coerce

-- | @subRoutes@ are valid sub-routes of @r@
type ValidSubRoutes r subRoutes =
  ( SameShapeAs (RCode r) subRoutes
  , SameShapeAs subRoutes (RCode r)
  , All Top (RCode r)
  , All Top subRoutes
  , AllZipF Coercible (RCode r) subRoutes
  , AllZipF Coercible subRoutes (RCode r)
  )

#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
type family GSubRoutes (name :: SOPM.DatatypeName) (constrs :: [SOPM.ConstructorName]) (xs :: [Type]) :: [Type] where
  GSubRoutes _ _ '[] = '[]
  GSubRoutes name (c ': cs) (() ': xs) =
    -- TODO: The .html suffix part should be overridable.
    FileRoute (Constructor2RoutePath name c ".html")
      ': GSubRoutes name cs xs
  GSubRoutes name (c ': cs) (x ': xs) =
    FolderRoute (Constructor2RoutePath name c "") x
      ': GSubRoutes name cs xs

type family
  Constructor2RoutePath
    (name :: SOPM.DatatypeName)
    (constr :: SOPM.ConstructorName)
    (suffix :: Symbol) ::
    Symbol
  where
  Constructor2RoutePath name constr suffix =
    AppendSymbol
      ( -- Instead of ToLower we want Camel2Kebab here, ideally.
        -- So that `Foo_BarQux` encodes to bar-qux instead of barqux.
        ToLower
          ( StripPrefix
              (AppendSymbol name "_")
              constr
          )
      )
      suffix
#else 
type family GSubRoutes (name :: SOPM.DatatypeName) (constrs :: [SOPM.ConstructorName]) (xs :: [Type]) :: [Type] where
  GSubRoutes _ _ _ = TypeError ('Text "GHC 9.2 is required for anyclass deriving of HasSubRoutes")
#endif

class HasSubRoutes r => HasSubModels r where
  -- | Break the model into a list of sub-models used correspondingly by the sub-routes.
  subModels :: RouteModel r -> NP I (MultiModel (SubRoutes r))

{- | DerivingVia type for HasSubModels

  The `lookups` are processed using `HasAny`'s `the`.
-}
newtype r `WithSubModels` (lookups :: [k]) = WithSubModels r
  deriving stock (Eq, Show)
  deriving newtype (IsRoute, HasSubRoutes)

instance
  ( HasSubRoutes (r `WithSubModels` lookups)
  , GSubModels
      (RouteModel r)
      (MultiModel (SubRoutes r))
      lookups
  ) =>
  HasSubModels (r `WithSubModels` (lookups :: [k]))
  where
  subModels m =
    gsubModels
      @_
      @(RouteModel r)
      @(MultiModel (SubRoutes r))
      @lookups
      m

class GSubModels m (ms :: [Type]) (lookups :: [k]) where
  gsubModels :: m -> NP I ms

instance GSubModels m '[] '[] where
  gsubModels _ = Nil

{-
instance {-# OVERLAPPABLE #-} GSubModels m ms ss => GSubModels m (() ': ms) (Const () ': ss) where
  gsubModels m = I () :* gsubModels @_ @m @ms @ss m

instance
  {-# OVERLAPPING #-}
  (Generic m, GSubModels m ms ss) =>
  GSubModels m (m ': ms) (Identity ': ss)
  where
  gsubModels m = I m :* gsubModels @_ @m @ms @ss m
-}

instance
  {-# OVERLAPPING #-}
  (HasAny s m m t t, GSubModels m ms ss) =>
  GSubModels m (t ': ms) (s ': ss)
  where
  gsubModels m = I (view (the @s @m @_ @t @_) m) :* gsubModels @_ @m @ms @ss m

-- Useful instances to support varied types in `WithSubModels` list.

instance {-# OVERLAPPING #-} HasAny () s s () () where
  the = united

instance HasAny sel s t a b => HasAny (Proxy sel) s t a b where
  the = the @sel

data The = TheUnit | TheId | TheField Symbol | TheType Type
instance {-# OVERLAPPING #-} HasAny 'TheUnit s s () () where
  the = united

instance {-# OVERLAPPING #-} HasAny 'TheId s s s s where
  the = castOptic equality

instance HasAny sel s t a b => HasAny ( 'TheField sel) s t a b where
  the = the @sel

instance HasAny sel s t a b => HasAny ( 'TheType sel) s t a b where
  the = the @sel
