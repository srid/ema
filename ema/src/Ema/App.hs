{-# LANGUAGE AllowAmbiguousTypes #-}

module Ema.App (
  SiteConfig (..),
  runSite,
  runSite_,
  runSiteWith,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad.Logger (LoggingT (runLoggingT), MonadLoggerIO (askLoggerIO), logInfoNS, logWarnNS)
import Control.Monad.Logger.Extras (runLoggerLoggingT)
import Data.Default (Default, def)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.LVar qualified as LVar
import Data.Some (Some (Some))
import Ema.CLI (getLogger)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic (Dynamic))
import Ema.Generate (generateSiteFromModel)
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Server qualified as Server
import Ema.Site (EmaSite (SiteArg, siteInput), EmaStaticSite)
import System.Directory (getCurrentDirectory)

data SiteConfig r = SiteConfig
  { siteConfigCli :: CLI.Cli
  , siteConfigServerOpts :: Server.EmaServerOptions r
  }

instance Default (SiteConfig r) where
  def =
    SiteConfig
      { siteConfigCli = def
      , siteConfigServerOpts = def
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
  IO [FilePath]
runSite input = do
  cli <- CLI.cliAction
  let cfg = SiteConfig cli def
  result <- snd <$> runSiteWith @r cfg input
  case result of
    CLI.Run _ :=> Identity () ->
      flip runLoggerLoggingT (getLogger cli) $
        CLI.crash "ema" "Live server unexpectedly stopped"
    CLI.Generate _ :=> Identity fs ->
      pure fs

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
    , DSum CLI.Action Identity
    )
runSiteWith cfg siteArg = do
  let opts = siteConfigServerOpts cfg
      cli = siteConfigCli cfg
  flip runLoggerLoggingT (getLogger cli) $ do
    cwd <- liftIO getCurrentDirectory
    logInfoNS "ema" $ "Launching Ema under: " <> toText cwd
    Dynamic (model0 :: RouteModel r, cont) <- siteInput @r (CLI.action cli) siteArg
    case CLI.action cli of
      Some act@(CLI.Generate dest) -> do
        fs <- generateSiteFromModel @r dest model0
        pure (model0, act :=> Identity fs)
      Some act@(CLI.Run (host, mport)) -> do
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
                Server.runServerWithWebSocketHotReload @r opts host mport model
            )
        pure (model0, act :=> Identity ())
