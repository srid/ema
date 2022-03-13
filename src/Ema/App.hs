{-# LANGUAGE AllowAmbiguousTypes #-}

module Ema.App
  ( runSite,
    runSiteWithCli,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad.Logger (logInfoNS, logWarnNS)
import Control.Monad.Logger.Extras
  ( colorize,
    logToStdout,
    runLoggerLoggingT,
  )
import Data.Dependent.Sum (DSum ((:=>)))
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Some (Some (Some))
import Ema.Asset
import Ema.CLI (Cli)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic (Dynamic))
import Ema.Generate (generateSite)
import Ema.Model (HasModel (ModelInput, modelDynamic))
import Ema.Route.Class (IsRoute (mkRouteEncoder))
import Ema.Server qualified as Server
import System.Directory (getCurrentDirectory)

-- TODO: Rename to `run` and `runWithCli`

-- | Run the given Ema site, and return the generated files.
--
-- On live-server mode, this function will never return.
runSite ::
  forall r.
  (Show r, Eq r, IsRoute r, RenderAsset r, HasModel r) =>
  ModelInput r ->
  IO (DSum CLI.Action Identity)
runSite input = do
  cli <- CLI.cliAction
  runSiteWithCli @r cli input

-- | Like @runSite@ but takes the CLI action
--
-- Useful if you are handling CLI arguments yourself.
runSiteWithCli ::
  forall r.
  (Show r, Eq r, IsRoute r, RenderAsset r, HasModel r) =>
  Cli ->
  ModelInput r ->
  IO (DSum CLI.Action Identity)
runSiteWithCli cli input = do
  -- TODO: Allow library users to control logging levels, or colors.
  let logger = colorize logToStdout
  model :: LVar a <- LVar.empty
  flip runLoggerLoggingT logger $ do
    cwd <- liftIO getCurrentDirectory
    let logSrc = "main"
    logInfoNS logSrc $ "Launching Ema under: " <> toText cwd
    logInfoNS logSrc "Waiting for initial model ..."
    let enc = mkRouteEncoder @r
    Dynamic (model0 :: a, cont) <- modelDynamic @r (CLI.action cli) enc input
    logInfoNS logSrc "... initial model is now available."
    case CLI.action cli of
      Some act@(CLI.Generate dest) -> do
        fs <- generateSite @r dest model0
        pure $ act :=> Identity fs
      Some act@(CLI.Run (host, port)) -> do
        LVar.set model model0
        liftIO $
          race_
            ( flip runLoggerLoggingT logger $ do
                cont $ LVar.set model
                logWarnNS logSrc "modelPatcher exited; no more model updates!"
                liftIO $ threadDelay maxBound
            )
            (flip runLoggerLoggingT logger $ Server.runServerWithWebSocketHotReload @r host port model)
        pure $ act :=> Identity ()
