{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Multi.Generic.Motley where

import Data.SOP.Constraint (AllZipF)
import Data.SOP.NS (trans_NS)
import Ema.Multi (MultiModel, MultiRoute)
import Ema.Multi.Generic.RGeneric (RConstructorNames, RDatatypeName, RGeneric (..))
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Route.Extra
import GHC.TypeLits (AppendSymbol, Symbol)
import GHC.TypeLits.Extra.Symbol (StripPrefix, ToLower)
import Generics.SOP
import Generics.SOP.Type.Metadata qualified as SOPM
import Optics.Core (
  Iso',
  iso,
 )
import Prelude hiding (All, Generic)

{- | HasSubRoutes is a class of routes with an underlying MultiRoute (and MultiModel) representation.

 The idea is that by deriving HasSubRoutes (and HasSubModels), we get IsRoute for free (based on MultiRoute).

 TODO: Rename this class, or change the API.
-}
class HasSubRoutes r where
  -- | The sub-routes in the `r` (for each constructor).
  type SubRoutes r :: [Type] -- TODO: Derive this generically

  type SubRoutes r = GSubRoutes (RDatatypeName r) (RConstructorNames r) (RCode r)

  subRoutesIso :: Iso' r (MultiRoute (SubRoutes r))
  default subRoutesIso ::
    ( RGeneric r
    , SameShapeAs (RCode r) (SubRoutes r)
    , SameShapeAs (SubRoutes r) (RCode r)
    , All Top (RCode r)
    , All Top (SubRoutes r)
    , AllZipF Coercible (RCode r) (SubRoutes r)
    , AllZipF Coercible (SubRoutes r) (RCode r)
    ) =>
    Iso' r (MultiRoute (SubRoutes r))
  subRoutesIso =
    iso (gtoSubRoutes @r . rfrom) (rto . gfromSubRoutes @r)

gtoSubRoutes ::
  forall r.
  ( RGeneric r
  , SameShapeAs (RCode r) (SubRoutes r)
  , SameShapeAs (SubRoutes r) (RCode r)
  , All Top (RCode r)
  , All Top (SubRoutes r)
  , AllZipF Coercible (RCode r) (SubRoutes r)
  ) =>
  NS I (RCode r) ->
  MultiRoute (SubRoutes r)
gtoSubRoutes = trans_NS (Proxy @Coercible) coerce

gfromSubRoutes ::
  forall r.
  ( RGeneric r
  , SameShapeAs (RCode r) (SubRoutes r)
  , SameShapeAs (SubRoutes r) (RCode r)
  , All Top (RCode r)
  , All Top (SubRoutes r)
  , AllZipF Coercible (SubRoutes r) (RCode r)
  ) =>
  MultiRoute (SubRoutes r) ->
  NS I (RCode r)
gfromSubRoutes = trans_NS (Proxy @Coercible) coerce

type family GSubRoutes (name :: SOPM.DatatypeName) (constrs :: [SOPM.ConstructorName]) (xs :: [Type]) :: [Type] where
  GSubRoutes _ _ '[] = '[]
  GSubRoutes name (c ': cs) (() ': xs) =
    -- TODO: The .html suffix part should be overridable.
    SingletonRoute (Constructor2RoutePath name c ".html")
      ': GSubRoutes name cs xs
  GSubRoutes name (c ': cs) (x ': xs) =
    PrefixedRoute (Constructor2RoutePath name c "") x
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

class HasSubRoutes r => HasSubModels r where
  -- | Break the model into a list of sub-models used correspondingly by the sub-routes.
  subModels :: RouteModel r -> NP I (MultiModel (SubRoutes r))
