{-# LANGUAGE AllowAmbiguousTypes #-}

module Ema.App (
  runSite,
  runSiteWithCli,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad.Logger (LoggingT (runLoggingT), MonadLoggerIO (askLoggerIO), logInfoNS, logWarnNS)
import Control.Monad.Logger.Extras (runLoggerLoggingT)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.LVar qualified as LVar
import Data.Some (Some (Some))
import Ema.Asset
import Ema.CLI (Cli, getLogger)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic (Dynamic))
import Ema.Generate (generateSite)
import Ema.Model (HasModel (ModelInput, modelDynamic))
import Ema.Route.Class (IsRoute (RouteModel, routeEncoder))
import Ema.Server qualified as Server
import System.Directory (getCurrentDirectory)

{- | Run the given Ema site, and return the generated files.

 On live-server mode, this function will never return.
-}
runSite ::
  forall r.
  (Show r, Eq r, IsRoute r, CanRender r, HasModel r, CanGenerate r) =>
  ModelInput r ->
  IO (DSum CLI.Action Identity)
runSite input = do
  cli <- CLI.cliAction
  snd <$> runSiteWithCli @r cli input

{- | Like @runSite@ but takes the CLI action

 Useful if you are handling CLI arguments yourself.
-}
runSiteWithCli ::
  forall r.
  (Show r, Eq r, IsRoute r, CanRender r, HasModel r, CanGenerate r) =>
  Cli ->
  ModelInput r ->
  IO (RouteModel r, DSum CLI.Action Identity)
runSiteWithCli cli input = do
  flip runLoggerLoggingT (getLogger cli) $ do
    cwd <- liftIO getCurrentDirectory
    logInfoNS "ema" $ "Launching Ema under: " <> toText cwd
    Dynamic (model0 :: RouteModel r, cont) <- modelDynamic @r (CLI.action cli) (routeEncoder @r) input
    case CLI.action cli of
      Some act@(CLI.Generate dest) -> do
        fs <- generateSite @r dest model0
        pure (model0, act :=> Identity fs)
      Some act@(CLI.Run (host, port)) -> do
        model <- LVar.empty
        LVar.set model model0
        logger <- askLoggerIO
        liftIO $
          race_
            ( flip runLoggingT logger $ do
                cont $ LVar.set model
                logWarnNS "ema" "modelPatcher exited; no more model updates!"
                -- We want to keep this thread alive, so that the server thread
                -- doesn't exit.
                liftIO $ threadDelay maxBound
            )
            ( flip runLoggingT logger $ do
                Server.runServerWithWebSocketHotReload @r host port model
            )
        pure (model0, act :=> Identity ())
