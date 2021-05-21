{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
class Ema route where
  -- How to convert URLs to/from routes
  encodeRoute :: route -> [Slug]
  decodeRoute :: [Slug] -> Maybe route

-- | The unit model is useful when using Ema in pure fashion (see @Ema.runEmaPure@) with a single route (index.html) only.
instance Ema () where
  encodeRoute () = []
  decodeRoute = \case
    [] -> Just ()
    _ -> Nothing
