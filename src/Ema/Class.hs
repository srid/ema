{-# LANGUAGE DefaultSignatures #-}

module Ema.Class where

-- | Enrich a model to work with Ema
-- TODO: Rename to `Routable`?
class Ema model where
  type RouteFor model :: Type

  -- | Get the filepath on disk corresponding to this route.
  encodeRoute :: model -> RouteFor model -> FilePath

  -- | Decode a filepath on disk into a route.
  decodeRoute :: model -> FilePath -> Maybe (RouteFor model)

  -- | All routes in the site
  --
  -- The `gen` command will generate only these routes. On live server, this
  -- function is never used.
  -- TODO: Rename to staticRoutes? Or be isomorphic to live server.
  allRoutes :: model -> [RouteFor model]
  default allRoutes :: (Bounded (RouteFor model), Enum (RouteFor model)) => model -> [RouteFor model]
  allRoutes _ = [minBound .. maxBound]

-- | The unit model is useful when using Ema in pure fashion (see
-- @Ema.runEmaPure@) with a single route (index.html) only.
instance Ema () where
  type RouteFor () = ()
  encodeRoute () () = []
  decodeRoute () = \case
    [] -> Just ()
    _ -> Nothing
  allRoutes () = one ()
