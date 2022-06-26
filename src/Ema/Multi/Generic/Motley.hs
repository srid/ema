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
import Ema.Route.Extra
import GHC.TypeLits (Symbol)
import Generics.SOP
import Generics.SOP.Type.Metadata qualified as SOPM
import Optics.Core (
  Iso',
  iso,
 )
import Prelude hiding (All, Generic)

{- | MotleyRoute is a class of routes with an underlying MultiRoute (and MultiModel) representation.

 The idea is that by deriving MotleyRoute (and MotleyModel), we get IsRoute for free (based on MultiRoute).

 TODO: Rename this class, or change the API.
-}
class MotleyRoute r where
  -- | The sub-routes in the `r` (for each constructor).
  type MotleyRouteSubRoutes r :: [Type] -- TODO: Derive this generically

  type MotleyRouteSubRoutes r = GMotleyRouteSubRoutes (RDatatypeName r) (RConstructorNames r) (RCode r)

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

type family GMotleyRouteSubRoutes (name :: SOPM.DatatypeName) (constrs :: [SOPM.ConstructorName]) (xs :: [Type]) :: [Type] where
  GMotleyRouteSubRoutes _ _ '[] = '[]
  GMotleyRouteSubRoutes name (c ': cs) (() ': xs) = SingletonRoute (Constructor2RoutePath name c) ': GMotleyRouteSubRoutes name cs xs
  GMotleyRouteSubRoutes name (c ': cs) (x ': xs) = PrefixedRoute (Constructor2RoutePath name c) x ': GMotleyRouteSubRoutes name cs xs

type family Constructor2RoutePath (name :: SOPM.DatatypeName) (constr :: SOPM.ConstructorName) :: Symbol where
-- TODO: replace 'constr' with "${toLower ${stripPrefix '${name}_' constr}}". This requires heavy type-level symbol processing.
  Constructor2RoutePath name constr = constr

class MotleyRoute r => MotleyModel r where
  type MotleyModelType r :: Type

  -- | Break the model into a list of sub-models used correspondingly by the sub-routes.
  motleySubModels :: MotleyModelType r -> NP I (MultiModel (MotleyRouteSubRoutes r))
