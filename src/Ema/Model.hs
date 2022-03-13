{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Model
  ( HasModel (..),
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Some (Some)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic)
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Route.Encoder (RouteEncoder)
import UnliftIO (MonadUnliftIO)

class IsRoute r => HasModel r where
  {- Arguments to the model runner. Default: nothing (hence, `()`)

    The arguments should be passed to `Ema.runSite`.
  -}
  type ModelInput r :: Type

  type ModelInput r = ()

  {- Get the model's time-varying value as a `Dynamic`.

    If your model is not time-varying, use `pure` to produce a constant value.
  -}
  modelDynamic ::
    forall m.
    (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
    Some CLI.Action ->
    RouteEncoder (RouteModel r) r ->
    ModelInput r ->
    m (Dynamic m (RouteModel r))
  default modelDynamic ::
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
  modelDynamic _ _ _ =
    -- The default implementation assumes the minimal model, `()`, which cannot
    -- be time-varying.
    pure $ pure ()
