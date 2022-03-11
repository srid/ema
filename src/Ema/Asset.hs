{-# LANGUAGE UndecidableInstances #-}

module Ema.Asset
  ( Asset (..),
    Format (..),
    RenderAsset (..),
  )
where

import Ema.Route.Class
import Ema.Route.Encoder

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

class IsRoute r => RenderAsset r where
  -- | Produce the asset for the given route.
  renderAsset ::
    RouteEncoder (RouteModel r) r -> RouteModel r -> r -> Asset LByteString

instance (RenderAsset r1, RenderAsset r2, IsRoute (Either r1 r2), RouteModel (Either r1 r2) ~ (RouteModel r1, RouteModel r2)) => RenderAsset (Either r1 r2) where
  renderAsset enc m = \case
    Left r -> renderAsset @r1 (leftRouteEncoder enc) (fst m) r
    Right r -> renderAsset @r2 (rightRouteEncoder enc) (snd m) r
