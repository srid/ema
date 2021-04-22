{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ema.Class where

import Ema.Route.Slug (Slug)

-- | Enrich a model to work with Ema
class Ema model where
  -- | The route type associated with this model
  type Route model

  -- How to convert URLs to/from routes
  encodeRoute :: Route model -> [Slug]
  decodeRoute :: [Slug] -> Maybe (Route model)

  -- | Compute all routes to generate given a model value
  --
  -- This is used only during static site generation (not dev server).
  modelRoutes :: model -> [Route model]

-- | The unit model is useful when using Ema in pure fashion (see @Ema.runEmaPure@) with a single route (index.html) only.
instance Ema () where
  type Route () = ()
  encodeRoute () = []
  decodeRoute = \case
    [] -> Just ()
    _ -> Nothing
  modelRoutes () = one ()
