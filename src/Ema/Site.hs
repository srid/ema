{-# LANGUAGE UndecidableInstances #-}

module Ema.Site
  ( RenderAsset (..),
    HasModel (..),
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Some (Some)
import Ema.Asset (Asset)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic (Dynamic))
import Ema.Route.Encoder
  ( RouteEncoder,
    leftRouteEncoder,
    rightRouteEncoder,
  )
import Ema.Route.Generic (IsRoute (RouteModel))
import UnliftIO (MonadUnliftIO)

class IsRoute r => HasModel r where
  type ModelInput r :: Type
  runModel ::
    forall m.
    ( MonadIO m,
      MonadUnliftIO m,
      MonadLoggerIO m
    ) =>
    Some CLI.Action ->
    RouteEncoder (RouteModel r) r ->
    ModelInput r ->
    m (Dynamic m (RouteModel r))

class IsRoute r => RenderAsset r where
  renderAsset ::
    RouteEncoder (RouteModel r) r ->
    RouteModel r ->
    r ->
    Asset LByteString

instance (RenderAsset r1, RenderAsset r2, IsRoute (Either r1 r2), RouteModel (Either r1 r2) ~ (RouteModel r1, RouteModel r2)) => RenderAsset (Either r1 r2) where
  renderAsset enc m = \case
    Left r -> renderAsset @r1 (leftRouteEncoder enc) (fst m) r
    Right r -> renderAsset @r2 (rightRouteEncoder enc) (snd m) r
