{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Generic.SubRoute (
  HasSubRoutes (SubRoutes),
  subRoutesIso,
  -- DerivingVia types
  GSubRoutes,
  gtoSubRoutes,
  gfromSubRoutes,
  ValidSubRoutes,
) where

import Data.SOP.Constraint (AllZipF)
import Data.SOP.NS (trans_NS)
import Ema.Route.Generic.RGeneric (RGeneric (..))
import Ema.Route.Lib.Multi (MultiRoute)
import GHC.TypeLits (AppendSymbol, Symbol)
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
import GHC.TypeLits.Extra.Symbol (StripPrefix, ToLower)
import Ema.Route.Lib.File (FileRoute)
import Ema.Route.Lib.Folder (FolderRoute)
#else 
import GHC.TypeLits
#endif
import Ema.Route.Generic.Iso (GIsomorphic (giso))
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

subRoutesIso ::
  forall r.
  ( RGeneric r
  , HasSubRoutes r
  , ValidSubRoutes r (SubRoutes r)
  ) =>
  Iso' r (MultiRoute (SubRoutes r))
subRoutesIso =
  iso (gtoSubRoutes @r . rfrom) (rto . gfromSubRoutes @r)

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
