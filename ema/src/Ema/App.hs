{-# LANGUAGE AllowAmbiguousTypes #-}

module Ema.App (
  SiteConfig (..),
  runSite,
  runSite_,
  runSiteWith,
  runSiteWithInput,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (MonadLoggerIO, logInfoNS, logWarnNS)
import Control.Monad.Logger.Extras (runLoggerLoggingT)
import Data.Default (Default, def)
import Data.LVar qualified as LVar
import Ema.CLI (getLogger)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic (Dynamic))
import Ema.Generate (generateSiteFromModel)
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Server qualified as Server
import Ema.Site (EmaSite (SiteArg, siteInput), EmaStaticSite)
import System.Directory (getCurrentDirectory)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (race_)

data SiteConfig r = SiteConfig
  { siteConfigCli :: CLI.Cli
  , siteConfigWebSocketOptions :: Server.EmaWebSocketOptions r
  }

instance Default (SiteConfig r) where
  def =
    SiteConfig
      { siteConfigCli = def
      , siteConfigWebSocketOptions = def
      }

{- | Run the given Ema site,

  Takes as argument the associated `SiteArg`.

  In generate mode, return the generated files.  In live-server mode, this
  function will never return.
-}
runSite ::
  forall r.
  (Show r, Eq r, EmaStaticSite r) =>
  -- | The input required to create the `Dynamic` of the `RouteModel`
  SiteArg r ->
  IO (FilePath, [FilePath])
runSite input = do
  cli <- CLI.cliAction
  let cfg = SiteConfig cli def
  snd <$> runSiteWith @r cfg input

-- | Like @runSite@ but discards the result
runSite_ :: forall r. (Show r, Eq r, EmaStaticSite r) => SiteArg r -> IO ()
runSite_ = void . runSite @r

{- | Like @runSite@ but takes custom @SiteConfig@.

 Useful if you are handling the CLI arguments yourself and/or customizing the
 server websocket handler.

 Use "void $ Ema.runSiteWith def ..." if you are running live-server only.
-}
runSiteWith ::
  forall r.
  (Show r, Eq r, EmaStaticSite r) =>
  SiteConfig r ->
  SiteArg r ->
  IO
    ( -- The initial model value.
      RouteModel r
    , -- Out path, and the list of statically generated files
      (FilePath, [FilePath])
    )
runSiteWith cfg siteArg = do
  let cli = siteConfigCli cfg
  flip runLoggerLoggingT (getLogger cli) $ do
    dyn <- siteInput @r (CLI.action cli) siteArg
    runSiteWithInput cfg dyn

{- | Like 'runSiteWith' but accepts a pre-built 'Dynamic' instead of
calling 'siteInput' internally.

Useful when you need to insert logic between the 'Dynamic''s construction
and its consumption — for example, to 'Ema.Dynamic.currentValue'-tee it
for an out-of-band consumer (HTTP API, metrics, test harness) or to
'Applicative'-compose it with another source.

'siteInput' still defines /how/ to build the 'Dynamic'; the caller just
invokes it explicitly, threads the result through whatever combinators
they need, and hands it here.

> do
>   dyn <- siteInput @r action arg              -- user's siteInput runs
>   (readModel, wrapped) <- currentValue dyn    -- tee for out-of-band reads
>   race_ (myObserver readModel)
>         (runSiteWithInput cfg wrapped)
-}
runSiteWithInput ::
  forall r m.
  -- MonadUnliftIO for the race_ between updater and server; MonadLoggerIO for
  -- Ema's own startup / patch / error logs.
  (MonadUnliftIO m, MonadLoggerIO m, Show r, Eq r, EmaStaticSite r) =>
  SiteConfig r ->
  Dynamic m (RouteModel r) ->
  m
    ( -- The initial model value.
      RouteModel r
    , -- Out path, and the list of statically generated files
      (FilePath, [FilePath])
    )
runSiteWithInput cfg (Dynamic (model0 :: RouteModel r, cont)) = do
  let opts = siteConfigWebSocketOptions cfg
      cli = siteConfigCli cfg
  cwd <- liftIO getCurrentDirectory
  logInfoNS "ema" $ "Launching Ema under: " <> toText cwd
  case CLI.action cli of
    CLI.Generate dest -> do
      fs <- generateSiteFromModel @r dest model0
      pure (model0, (dest, fs))
    CLI.Run runArgs -> do
      let host = CLI.host runArgs
          mport = CLI.port runArgs
          noWebSocket = CLI.unNoWebSocket (CLI.noWebSocket runArgs)
          mWsOpts = if noWebSocket then Nothing else Just opts
      model <- LVar.empty
      LVar.set model model0
      race_
        ( do
            cont $ LVar.set model
            logWarnNS "ema" "modelPatcher exited; no more model updates!"
            -- We want to keep this thread alive, so that the server thread
            -- doesn't exit.
            liftIO $ threadDelay maxBound
        )
        (Server.runServerWithWebSocketHotReload @r mWsOpts host mport model)
      CLI.crash "ema" "Live server unexpectedly stopped"
