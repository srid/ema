{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ema.Class where

import Ema.Route.Slug

class Ema model where
  type Route model

  -- How to convert browser URLs to/from routes
  encodeRoute :: Route model -> [Slug]
  decodeRoute :: [Slug] -> Maybe (Route model)

  -- | Compute all routes to generate given a model value
  modelRoutes :: model -> [Route model]

instance Ema () where
  type Route () = ()
  encodeRoute () = []
  decodeRoute = \case
    [] -> Just ()
    _ -> Nothing
  modelRoutes () = one ()
