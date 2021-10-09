{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Class where

-- | Enrich a model to work with Ema
class Ema model route | route -> model where
  -- | Get the filepath on disk corresponding to this route.
  encodeRoute :: model -> route -> FilePath

  -- | Decode a filepath on disk into a route.
  decodeRoute :: model -> FilePath -> Maybe route

  -- | All routes in the site
  --
  -- The `gen` command will generate only these routes. On live server, this
  -- function is never used.
  allRoutes :: model -> [route]
  default allRoutes :: (Bounded route, Enum route) => model -> [route]
  allRoutes _ = [minBound .. maxBound]

-- | The unit model is useful when using Ema in pure fashion (see
-- @Ema.runEmaPure@) with a single route (index.html) only.
instance Ema () () where
  encodeRoute () () = []
  decodeRoute () = \case
    [] -> Just ()
    _ -> Nothing
  allRoutes () = one ()
