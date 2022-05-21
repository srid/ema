{-# LANGUAGE AllowAmbiguousTypes #-}

module Ema.App (
  runSite,
  runSiteWithCli,
  runSiteLiveServerOnly,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad.Logger (LoggingT (runLoggingT), MonadLoggerIO (askLoggerIO), logInfoNS, logWarnNS)
import Control.Monad.Logger.Extras (runLoggerLoggingT)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.LVar qualified as LVar
import Data.Some (Some (Some))
import Ema.CLI (Action (Run), Cli (Cli), getLogger)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic (Dynamic))
import Ema.Generate (generateSite)
import Ema.Route.Class (IsRoute (RouteModel, routeEncoder))
import Ema.Server qualified as Server
import Ema.Site (EmaSite (SiteArg, siteInput))
import System.Directory (getCurrentDirectory)

{- | Run the given Ema site,

  Takes as argument the associated `SiteArg`.

  In generate mode, return the generated files.  On live-server mode, this
  function will never return.
-}
runSite ::
  forall r.
  (Show r, Eq r, EmaSite r) =>
  -- | The input required to create the `Dynamic` of the `ModelRoute`
  SiteArg r ->
  IO (DSum CLI.Action Identity)
runSite input = do
  cli <- CLI.cliAction
  snd <$> runSiteWithCli @r cli input

{- | Like @runSite@ but takes the CLI action

 Useful if you are handling the CLI arguments yourself.
-}
runSiteWithCli ::
  forall r.
  (Show r, Eq r, EmaSite r) =>
  Cli ->
  SiteArg r ->
  IO (RouteModel r, DSum CLI.Action Identity)
runSiteWithCli cli input = do
  flip runLoggerLoggingT (getLogger cli) $ do
    cwd <- liftIO getCurrentDirectory
    logInfoNS "ema" $ "Launching Ema under: " <> toText cwd
    Dynamic (model0 :: RouteModel r, cont) <- siteInput @r (CLI.action cli) (routeEncoder @r) input
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

{- | Like `runSiteWithCli` but only runs the live server.

Also discards the result, inasmuch as live server never returns.

Note that static generation is evidently disabled. Useful for apps that need a
live view, but do not care about actually generating the files.
-}
runSiteLiveServerOnly ::
  forall r.
  (Show r, Eq r, EmaSite r) =>
  CLI.Host ->
  CLI.Port ->
  SiteArg r ->
  IO ()
runSiteLiveServerOnly host port siteArg =
  let emaCli = Cli (Some $ Run (host, port)) False
   in void $ runSiteWithCli @r emaCli siteArg
