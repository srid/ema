{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ema.App
  ( runEma,
    runEmaPure,
    runEmaWithCli,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad.Logger (MonadLoggerIO, logInfoN)
import Control.Monad.Logger.Extras
  ( colorize,
    logToStdout,
    runLoggerLoggingT,
  )
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import Ema.Asset (Asset (AssetGenerated), Format (Html))
import Ema.CLI (Cli)
import qualified Ema.CLI as CLI
import Ema.Class (Ema)
import qualified Ema.Generate as Generate
import qualified Ema.Server as Server
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import UnliftIO (BufferMode (BlockBuffering, LineBuffering), MonadUnliftIO, hFlush, hSetBuffering)

-- | Pure version of @runEmaWith@ (i.e with no model).
--
-- Due to purity, there is no impure state, and thus no time-varying model.
-- Neither is there a concept of route, as only a single route (index.html) is
-- expected, whose HTML contents is specified as the only argument to this
-- function.
runEmaPure ::
  -- | How to render a route
  (CLI.Action -> LByteString) ->
  IO ()
runEmaPure render = do
  void $
    runEma (\act () () -> AssetGenerated Html $ render act) $ \act model -> do
      LVar.set model ()
      when (act == CLI.Run) $ do
        liftIO $ threadDelay maxBound

-- | Convenient version of @runEmaWith@ that takes initial model and an update
-- function. You typically want to use this.
--
-- It uses @race_@ to properly clean up the update action when the ema thread
-- exits, and vice-versa.
runEma ::
  forall model route b.
  (Ema model route, Show route) =>
  -- | How to render a route, given the model
  (CLI.Action -> model -> route -> Asset LByteString) ->
  -- | A long-running IO action that will update the @model@ @LVar@ over time.
  -- This IO action must set the initial model value in the very beginning.
  (forall m. (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) => CLI.Action -> LVar model -> m b) ->
  IO (Either b (Maybe [FilePath]))
runEma render runModel = do
  cli <- CLI.cliAction
  runEmaWithCli cli render runModel

-- | Like @runEma@ but takes the CLI action
--
-- Useful if you are handling CLI arguments yourself.
runEmaWithCli ::
  forall model route b.
  (Ema model route, Show route) =>
  Cli ->
  -- | How to render a route, given the model
  (CLI.Action -> model -> route -> Asset LByteString) ->
  -- | A long-running IO action that will update the @model@ @LVar@ over time.
  -- This IO action must set the initial model value in the very beginning.
  (forall m. (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) => CLI.Action -> LVar model -> m b) ->
  IO (Either b (Maybe [FilePath]))
runEmaWithCli cli render runModel = do
  model <- LVar.empty
  -- TODO: Allow library users to control logging levels, or colors.
  let logger = colorize logToStdout
  flip runLoggerLoggingT logger $ do
    cwd <- liftIO getCurrentDirectory
    logInfoN $ "Launching Ema under: " <> toText cwd
    logInfoN "Waiting for initial model ..."
  race
    (flip runLoggerLoggingT logger $ runModel (CLI.action cli) model)
    (flip runLoggerLoggingT logger $ runEmaWithCliInCwd (CLI.action cli) model render)

-- | Run Ema live dev server
runEmaWithCliInCwd ::
  forall model route m.
  (MonadIO m, MonadUnliftIO m, MonadLoggerIO m, Ema model route, Show route) =>
  -- | CLI arguments
  CLI.Action ->
  -- | Your site model type, as a @LVar@ in order to support modifications over
  -- time (for hot-reload).
  --
  -- Use @Data.LVar.new@ to create it, and then -- over time -- @Data.LVar.set@
  -- or @Data.LVar.modify@ to modify it. Ema will automatically hot-reload your
  -- site as this model data changes.
  LVar model ->
  -- | Your site render function. Takes the current @model@ value, and the page
  -- @route@ type as arguments. It must return the raw HTML to render to browser
  -- or generate on disk.
  (CLI.Action -> model -> route -> Asset LByteString) ->
  m (Maybe [FilePath])
runEmaWithCliInCwd cliAction model render = do
  val <- LVar.get model
  logInfoN "... initial model is now available."
  case cliAction of
    CLI.Generate dest -> do
      fmap Just <$> withBlockBuffering $
        Generate.generate dest val (render cliAction)
    CLI.Run -> do
      port <- liftIO $ fromMaybe 8000 . (readMaybe @Int =<<) <$> lookupEnv "PORT"
      host <- liftIO $ fromMaybe "127.0.0.1" <$> lookupEnv "HOST"
      Server.runServerWithWebSocketHotReload host port model (render cliAction)
      pure Nothing
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)
