{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Model
  ( HasModel (..),
    unitModel,
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Some (Some)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic (Dynamic))
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Route.Encoder (RouteEncoder)
import UnliftIO (MonadUnliftIO)

class IsRoute r => HasModel r where
  type ModelInput r :: Type
  type ModelInput r = ()
  runModel ::
    forall m.
    (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
    Some CLI.Action ->
    RouteEncoder (RouteModel r) r ->
    ModelInput r ->
    m (Dynamic m (RouteModel r))
  default runModel ::
    forall m.
    ( MonadIO m,
      MonadUnliftIO m,
      MonadLoggerIO m,
      RouteModel r ~ ()
    ) =>
    Some CLI.Action ->
    RouteEncoder (RouteModel r) r ->
    ModelInput r ->
    m (Dynamic m (RouteModel r))
  runModel _ _ _ = pure unitModel

unitModel :: Monad m => Dynamic m ()
unitModel =
  Dynamic
    ( (),
      \_set -> do
        pure ()
    )
