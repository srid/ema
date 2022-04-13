{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Model (
  HasModel (..),
) where

import Control.Monad.Logger (MonadLoggerIO)
import Data.SOP
import Data.Some (Some)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic)
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Route.Encoder (RouteEncoder)
import UnliftIO (MonadUnliftIO)

{- | Class of routes with an associated model

  The default implementation works with generic deriving of `IsRoute`.
-}
class IsRoute r => HasModel r where
  {- Arguments to the model runner. Default: nothing (hence, `()`)

    The value of `ModelInput` should be passed to `Ema.runSite`. It is whatever
    data that is required to create the model `Dynamic` using `modelDynamic`.

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
    -- | The `RouteEncoder` associated with `r`
    RouteEncoder (RouteModel r) r ->
    ModelInput r ->
    m (Dynamic m (RouteModel r))
  default modelDynamic ::
    forall m.
    ( MonadIO m
    , MonadUnliftIO m
    , MonadLoggerIO m
    , RouteModel r ~ NP I '[]
    ) =>
    Some CLI.Action ->
    RouteEncoder (RouteModel r) r ->
    ModelInput r ->
    m (Dynamic m (RouteModel r))
  modelDynamic _ _ _ =
    -- The default implementation assumes the constant unit model which cannot
    -- be time-varying.
    pure $ pure Nil
