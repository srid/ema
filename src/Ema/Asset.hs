{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Asset (
  Asset (..),
  Format (..),
  CanRender (..),
  CanGenerate (..),
) where

import Data.SOP.NP (cpure_POP)
import Data.Set qualified as Set
import Ema.Route.Class (
  Contains (npIso),
  GRouteModel,
  IsRoute (RouteModel),
  IsRouteIn,
  IsRouteProd,
  NPConst (npConstFrom),
  SingleModelRoute (SingleModelRoute),
 )
import Ema.Route.Encoder (RouteEncoder)
import Generics.SOP hiding (Generic)
import Optics.Core (view)
import Prelude hiding (All)

-- | The type of assets that can be bundled in a static site.
data Asset a
  = -- | A file that is copied as-is from the source directory.
    --
    -- Relative paths are assumed relative to the source directory. Absolute
    -- paths allow copying static files outside of source directory.
    AssetStatic FilePath
  | -- | A file whose contents are generated at runtime by user code.
    AssetGenerated Format a
  deriving stock (Eq, Show, Ord, Functor, Generic)

-- | The format of a generated asset.
data Format
  = -- | Html assets are served by the server with hot-reload
    Html
  | -- | Other assets are served by the server as static files.
    Other
  deriving stock (Eq, Show, Ord, Generic)

{- | Class of routes that can be rendered as assets.

  All routes should have this instance, derived manually indicating how to
  render them.
-}
class IsRoute r => CanRender r where
  -- | Produce the asset for the given route.
  routeAsset :: RouteEncoder (RouteModel r) r -> RouteModel r -> r -> Asset LByteString

{- | Class of routes to statically generate.

  For routes with dynamic models you typically want to derive this manually for
  some inner routes and use generic deriving for the rest.
-}
class IsRoute r => CanGenerate r where
  generatableRoutes :: RouteModel r -> [r]
  -- The default implementation uses generics to compute the enumeration
  -- recursively, while ignoring the model.
  default generatableRoutes ::
    ( IsRoute r
    , All2 CanGenerate (Code r)
    , All2 IsRoute (Code r)
    , All2 (IsGeneratableProd ms) (Code r)
    , HasDatatypeInfo r
    , ms ~ GRouteModel (Code r)
    , All (IsRouteProd ms) (Code r)
    , RouteModel r ~ NP I ms
    , Ord r
    ) =>
    RouteModel r ->
    [r]
  generatableRoutes = gGeneratableRoutes

instance
  ( NPConst I (GRouteModel (Code r)) m
  , HasDatatypeInfo r
  , All2 CanGenerate (Code r)
  , All2 IsRoute (Code r)
  , All2 (IsGeneratableProd ms) (Code r)
  , ms ~ GRouteModel (Code r)
  , All (IsRouteProd ms) (Code r)
  , IsRoute r
  , Ord r
  ) =>
  CanGenerate (SingleModelRoute m r)
  where
  generatableRoutes m =
    SingleModelRoute <$> gGeneratableRoutes @r (npConstFrom . I $ m)

class (IsRouteIn ms r, CanGenerate r) => IsGeneratableProd ms r
instance (IsRouteIn ms r, CanGenerate r) => IsGeneratableProd ms r

gGeneratableRoutes ::
  forall r ms.
  ( IsRoute r
  , All2 CanGenerate (Code r)
  , All2 IsRoute (Code r)
  , All2 (IsGeneratableProd ms) (Code r)
  , HasDatatypeInfo r
  , ms ~ GRouteModel (Code r)
  , All (IsRouteProd ms) (Code r)
  , Ord r
  ) =>
  NP I ms ->
  [r]
gGeneratableRoutes m =
  let pop =
        cpure_POP
          (Proxy @(IsGeneratableProd ms))
          insideRoutes
      -- FIXME: Can we use traverse due to Applicative instance list.
      pops =
        hcfor
          (Proxy @IsRoute)
          pop
          id
      -- Workaround duplicates routes frmo the FIXME above.
      removeDups = Set.toList . Set.fromList
   in removeDups $ to <$> concatMap apInjs_POP pops
  where
    insideRoutes :: forall b. (IsGeneratableProd ms b) => [b]
    insideRoutes =
      let m' = view (npIso @_ @(RouteModel b)) m
       in generatableRoutes m'
