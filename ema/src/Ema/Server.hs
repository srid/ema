module Ema.Server (
  EmaWebSocketOptions (..),
  runServerWithWebSocketHotReload,
) where

import Control.Monad.Logger
import Data.LVar (LVar)
import Ema.CLI (Host (unHost))
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Server.HTTP (httpApp)
import Ema.Server.WebSocket (wsApp)
import Ema.Server.WebSocket.Options (EmaWebSocketOptions (..))
import Ema.Site (EmaStaticSite)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.WebSockets qualified as WS
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)

runServerWithWebSocketHotReload ::
  forall r m.
  ( Show r
  , MonadIO m
  , MonadUnliftIO m
  , MonadLoggerIO m
  , Eq r
  , IsRoute r
  , EmaStaticSite r
  ) =>
  Maybe (EmaWebSocketOptions r) ->
  Host ->
  Maybe Port ->
  LVar (RouteModel r) ->
  m ()
runServerWithWebSocketHotReload mWsOpts host mport model = do
  logger <- askLoggerIO
  let runM = flip runLoggingT logger
      settings =
        Warp.defaultSettings
          & Warp.setHost (fromString . toString . unHost $ host)
      app =
        case mWsOpts of
          Nothing ->
            httpApp @r logger model Nothing
          Just opts ->
            WaiWs.websocketsOr
              WS.defaultConnectionOptions
              (wsApp @r logger model $ emaWebSocketServerHandler opts)
              (httpApp @r logger model $ Just $ emaWebSocketClientShim opts)
      banner port = do
        logInfoNS "ema" "==============================================="
        logInfoNS "ema" $ "Ema live server RUNNING: http://" <> unHost host <> ":" <> show port <> " (" <> maybe "no ws" (const "ws") mWsOpts <> ")"
        logInfoNS "ema" "==============================================="
  liftIO $ warpRunSettings settings mport (runM . banner) app

-- Like Warp.runSettings but takes *optional* port. When no port is set, a
-- free (random) port is used.
warpRunSettings :: Warp.Settings -> Maybe Port -> (Port -> IO a) -> Wai.Application -> IO ()
warpRunSettings settings mPort banner app = do
  case mPort of
    Nothing ->
      Warp.withApplicationSettings settings (pure app) $ \port -> do
        void $ banner port
        threadDelay maxBound
    Just port -> do
      void $ banner port
      Warp.runSettings (settings & Warp.setPort port) app
