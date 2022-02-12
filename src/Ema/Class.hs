{-# LANGUAGE DefaultSignatures #-}

module Ema.Class (Ema (ModelFor)) where

-- | Enrich a model to work with Ema
-- TODO: Rename to `Routable`? (keeping only encode/decode)
class Ema r where
  type ModelFor r :: Type

instance Ema () where
  type ModelFor () = ()

{-
  -- | Get the filepath on disk corresponding to this route.
  encodeRoute :: ModelFor r -> r -> FilePath

  -- | Decode a filepath on disk into a route.
  decodeRoute :: ModelFor r -> FilePath -> Maybe r

  -- | All routes in the site
  --
  -- The `gen` command will generate only these routes. On live server, this
  -- function is never used.
  -- TODO: Rename to staticRoutes? Or be isomorphic to live server.
  -- TODO: Move to `Site` type
  allRoutes :: ModelFor r -> [r]
  default allRoutes :: (Bounded r, Enum r) => ModelFor r -> [r]
  allRoutes _ = [minBound .. maxBound]

-- | The unit model is useful when using Ema in pure fashion (see
-- @Ema.runEmaPure@) with a single route (index.html) only.
instance Ema () where
  type ModelFor () = ()
  encodeRoute () () = "index.html"
  decodeRoute () = \case
    "index.html" -> Just ()
    _ -> Nothing
  allRoutes () = one ()

-}
