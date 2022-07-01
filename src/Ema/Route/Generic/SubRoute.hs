{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Generic.SubRoute (
  HasSubRoutes (SubRoutes, subRoutesIso'),
  subRoutesIso,
  -- DerivingVia types
  WithSubRoutes (WithSubRoutes),
  -- Export these for DerivingVia coercion representations
  FileRoute (FileRoute),
  FolderRoute (FolderRoute),
) where

import Data.SOP.Constraint (AllZipF)
import Data.SOP.NS (trans_NS)
import Ema.Route.Generic.RGeneric (RConstructorNames, RDatatypeName, RGeneric (..))
import Ema.Route.Lib.File (FileRoute (FileRoute))
import Ema.Route.Lib.Folder (FolderRoute (FolderRoute))
import Ema.Route.Lib.Multi (MultiRoute)
import GHC.TypeLits (AppendSymbol, Symbol)
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
import GHC.TypeLits.Extra.Symbol (StripPrefix, ToLower)
#else 
import GHC.TypeLits
#endif
import GHC.Generics qualified as GHC
import Generics.SOP (All, I (..), NS, SameShapeAs, Top, unI)
import Generics.SOP.Type.Metadata qualified as SOPM
import Optics.Core (Iso', iso, view)
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
gtoSubRoutes = trans_NS (Proxy @GIsomorphic) (I . view giso . unI)

gfromSubRoutes ::
  forall r subRoutes.
  ( RGeneric r
  , ValidSubRoutes r subRoutes
  ) =>
  MultiRoute subRoutes ->
  NS I (RCode r)
gfromSubRoutes = trans_NS (Proxy @GIsomorphic) (I . view giso . unI)

-- | @subRoutes@ are valid sub-routes of @r@
type ValidSubRoutes r subRoutes =
  ( SameShapeAs (RCode r) subRoutes
  , SameShapeAs subRoutes (RCode r)
  , All Top (RCode r)
  , All Top subRoutes
  , AllZipF GIsomorphic (RCode r) subRoutes
  , AllZipF GIsomorphic subRoutes (RCode r)
  )

{- | Types `a` and `b` are isomorphic via their generic representation

  Unlike `Coercible` this constraint does not require that the two types have
  identical *runtime* representation. For example, `data Foo = Foo` is not
  coercible to `()` (they have different runtime representations), but they are
  both isomorphic via their generic representation.
-}
class (Generic a, Generic b, GHC.Rep a () `Coercible` GHC.Rep b ()) => GIsomorphic a b where
  giso :: Iso' a b

instance (Generic a, Generic b, GHC.Rep a () `Coercible` GHC.Rep b ()) => GIsomorphic a b where
  giso =
    iso
      (GHC.to @b @() . coerce . GHC.from @a @())
      (GHC.to @a @() . coerce . GHC.from @b @())

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
