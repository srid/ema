{-# LANGUAGE UndecidableInstances #-}

module Ema.Asset
  ( Asset (..),
    Format (..),
    HasAsset (..),
  )
where

import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Route.Encoder
  ( RouteEncoder,
    leftRouteEncoder,
    rightRouteEncoder,
  )

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
class IsRoute r => HasAsset r where
  -- | Produce the asset for the given route.
  routeAsset :: RouteEncoder (RouteModel r) r -> RouteModel r -> r -> Asset LByteString

  generatableRoutes :: RouteModel r -> [r]

-- Combining of two routes
instance
  (HasAsset r1, HasAsset r2, IsRoute (Either r1 r2), RouteModel (Either r1 r2) ~ (RouteModel r1, RouteModel r2)) =>
  HasAsset (Either r1 r2)
  where
  routeAsset enc m = \case
    Left r -> routeAsset @r1 (leftRouteEncoder enc) (fst m) r
    Right r -> routeAsset @r2 (rightRouteEncoder enc) (snd m) r
  generatableRoutes m =
    fmap Left (generatableRoutes @r1 $ fst m)
      <> fmap Right (generatableRoutes @r2 $ snd m)

{-
instance HasAsset (NS I rs) where
  routeAsset enc m r =
    undefined
-}
