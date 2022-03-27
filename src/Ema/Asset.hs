{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Asset (
  Asset (..),
  Format (..),
  CanRender (..),
  CanGenerate (..),
) where

import Data.SOP.NP
import Data.SOP.NS
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
import Ema.Route.Encoder (
  RouteEncoder,
  leftRouteEncoder,
  rightRouteEncoder,
 )
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

data Format = Html | Other
  deriving stock (Eq, Show, Ord, Generic)

-- | This route has assets associated with it.
class IsRoute r => CanRender r where
  -- | Produce the asset for the given route.
  routeAsset :: RouteEncoder (RouteModel r) r -> RouteModel r -> r -> Asset LByteString

-- | Class of routes to statically generate.
class IsRoute r => CanGenerate r where
  generatableRoutes :: RouteModel r -> [r]
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

-- Combining of two routes
instance
  (CanRender r1, CanRender r2, IsRoute (Either r1 r2), RouteModel (Either r1 r2) ~ (RouteModel r1, RouteModel r2)) =>
  CanRender (Either r1 r2)
  where
  routeAsset enc m = \case
    Left r -> routeAsset @r1 (leftRouteEncoder enc) (fst m) r
    Right r -> routeAsset @r2 (rightRouteEncoder enc) (snd m) r

instance
  (CanGenerate r1, CanGenerate r2, IsRoute (Either r1 r2), RouteModel (Either r1 r2) ~ (RouteModel r1, RouteModel r2)) =>
  CanGenerate (Either r1 r2)
  where
  generatableRoutes m =
    fmap Left (generatableRoutes @r1 $ fst m)
      <> fmap Right (generatableRoutes @r2 $ snd m)

{-
instance CanRender (NS I rs) where
  routeAsset enc m r =
    undefined
-}
