{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ema.Class where

import Ema.Route.Slug (Slug)

-- | Enrich a model to work with Ema
class Ema model route | route -> model where
  -- | Convert a route to URL slugs
  encodeRoute :: route -> [Slug]

  -- | Interpret the given URL slugs into a @route@
  --
  -- This function accepts a @model@ because more than one slug path may map to
  -- a route, and that choice could depend on the current model value (alias
  -- table, for instance). In most cases, the @model@ argument will be ignore.
  --
  -- NOTE: This function is used only the dev server. But you may invoke it in
  -- the render function in order to validate the links in the generated HTML.
  decodeRoute :: model -> [Slug] -> Maybe route

  -- | Routes to use when generating the static site
  --
  -- NOTE: This function is used only during static site generation.
  staticRoutes :: model -> [route]

  -- | List of (top-level) filepaths to serve as static assets
  --
  -- These will be copied over as-is during static site generation
  staticAssets :: Proxy route -> [FilePath]
  staticAssets Proxy = mempty

-- | The unit model is useful when using Ema in pure fashion (see @Ema.runEmaPure@) with a single route (index.html) only.
instance Ema () () where
  encodeRoute () = []
  decodeRoute () = \case
    [] -> Just ()
    _ -> Nothing
  staticRoutes () = one ()
