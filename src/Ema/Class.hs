{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ema.Class where

import Ema.Route.Slug (Slug)

-- | Enrich a model to work with Ema
class Ema model route | route -> model where
  -- How to convert URLs to/from routes
  encodeRoute :: route -> [Slug]
  decodeRoute :: [Slug] -> Maybe route

  -- | Compute all routes to generate given a model value
  --
  -- This is used only during static site generation (not dev server).
  modelRoutes :: model -> [route]

-- | The unit model is useful when using Ema in pure fashion (see @Ema.runEmaPure@) with a single route (index.html) only.
instance Ema () () where
  encodeRoute () = []
  decodeRoute = \case
    [] -> Just ()
    _ -> Nothing
  modelRoutes () = one ()
