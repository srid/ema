module Ema.Site
  ( Site (..),
    mkSite,
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Some (Some)
import Ema.Asset (Asset)
import Ema.CLI qualified as CLI
import Ema.Class
import UnliftIO
  ( MonadUnliftIO,
  )

data Site model b = Site
  { siteData :: LVar model,
    siteRun :: forall m. (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) => Some CLI.Action -> LVar model -> m b,
    siteRender :: Some CLI.Action -> model -> RouteFor model -> Asset LByteString
  }

mkSite ::
  forall model m b.
  MonadIO m =>
  ( Some CLI.Action ->
    model ->
    RouteFor model ->
    Asset LByteString
  ) ->
  (forall m1. MonadIO m1 => Some CLI.Action -> LVar model -> m1 b) ->
  m (Site model b)
mkSite render run = do
  model <- LVar.empty
  pure $ Site model run render
