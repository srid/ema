{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ema.Class where

import Control.Monad.Logger (MonadLoggerIO)
import Ema.Route.Slug (Slug)
import UnliftIO (MonadUnliftIO)

type MonadEma m =
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLoggerIO m
  )

-- | Enrich a model to work with Ema
class Ema model route | route -> model where
  -- How to convert URLs to/from routes
  encodeRoute :: route -> [Slug]
  decodeRoute :: [Slug] -> Maybe route

  -- | Routes to use when generating the static site
  --
  -- This is never used by the dev server.
  staticRoutes :: model -> [route]

  -- | List of (top-level) filepaths to serve as static assets
  --
  -- These will be copied over as-is during static site generation
  staticAssets :: Proxy route -> [FilePath]
  staticAssets Proxy = mempty

-- | The unit model is useful when using Ema in pure fashion (see @Ema.runEmaPure@) with a single route (index.html) only.
instance Ema () () where
  encodeRoute () = []
  decodeRoute = \case
    [] -> Just ()
    _ -> Nothing
  staticRoutes () = one ()
