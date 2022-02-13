module Ema.App
  ( runEma,
    runEmaPure,
    runEmaWithCli,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad.Logger (logInfoN)
import Control.Monad.Logger.Extras
  ( colorize,
    logToStdout,
    runLoggerLoggingT,
  )
import Data.LVar qualified as LVar
import Data.Some (Some (..))
import Ema.CLI (Cli)
import Ema.CLI qualified as CLI
import Ema.Generate (generateSite)
import Ema.Server qualified as Server
import Ema.Site
import System.Directory (getCurrentDirectory)
import Prelude

-- | Pure version of @runEmaWith@ (i.e with no model).
--
-- Due to purity, there is no impure state, and thus no time-varying model.
-- Neither is there a concept of route, as only a single route (index.html) is
-- expected, whose HTML contents is specified as the only argument to this
-- function.
runEmaPure ::
  -- | How to render the only route
  (Some CLI.Action -> LByteString) ->
  IO ()
runEmaPure render = do
  void $ runEma $ singlePageSite render

-- | Convenient version of @runEmaWith@ that takes initial model and an update
-- function. You typically want to use this.
--
-- It uses @race_@ to properly clean up the update action when the ema thread
-- exits, and vice-versa.
runEma ::
  forall r a.
  (Show r) =>
  Site r a ->
  -- | How to render a route, given the model
  -- (Some CLI.Action -> model -> RouteFor model -> Asset LByteString) ->
  -- | A long-running IO action that will update the @model@ @LVar@ over time.
  -- This IO action must set the initial model value in the very beginning.
  -- (forall m. (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) => Some CLI.Action -> LVar model -> m b) ->
  IO [FilePath]
runEma site = do
  cli <- CLI.cliAction
  runEmaWithCli cli site

-- | Like @runEma@ but takes the CLI action
--
-- Useful if you are handling CLI arguments yourself.
runEmaWithCli ::
  forall r a.
  (Show r) =>
  Cli ->
  -- | How to render a route, given the model
  Site r a ->
  -- | Returns generated files. On live-server mode, this function never returns.
  IO [FilePath]
runEmaWithCli cli site = do
  -- TODO: Allow library users to control logging levels, or colors.
  let logger = colorize logToStdout
  flip runLoggerLoggingT logger $ do
    cwd <- liftIO getCurrentDirectory
    logInfoN $ "Launching Ema under: " <> toText cwd
    logInfoN "Waiting for initial model ..."
  model <- LVar.empty
  flip runLoggerLoggingT logger $
    siteModelPatcher site (CLI.action cli) $ \model0 cont -> do
      LVar.set model model0
      case CLI.action cli of
        Some (CLI.Generate dest) -> do
          generateSite (CLI.action cli) dest site model
        Some (CLI.Run (host, port)) -> do
          liftIO $
            race_
              ( flip runLoggerLoggingT logger $ do
                  cont model
                  logInfoN "modelPatcher exited; keeping thread alive ..."
                  liftIO $ threadDelay maxBound
              )
              (flip runLoggerLoggingT logger $ Server.runServerWithWebSocketHotReload (CLI.action cli) host port site model)
          pure [] -- FIXME: err, unreachable
