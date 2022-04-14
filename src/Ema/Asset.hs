{-# LANGUAGE UndecidableInstances #-}

module Ema.Asset (
  Asset (..),
  Format (..),
  CanRender (..),
) where

import Ema.Route.Class (
  IsRoute (RouteModel),
 )
import Ema.Route.Encoder (RouteEncoder)
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
