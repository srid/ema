module Ema.Server.WebSocket where

import Control.Monad.Logger
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Ema.Asset (
  Asset (AssetGenerated, AssetStatic),
  Format (Html, Other),
 )
import Ema.Route.Class (IsRoute (RouteModel, routePrism))
import Ema.Route.Prism (
  fromPrism_,
 )
import Ema.Server.Common
import Ema.Server.WebSocket.Options (EmaWsHandler (..))
import Ema.Site (EmaStaticSite)
import Network.WebSockets (ConnectionException)
import Network.WebSockets qualified as WS
import Optics.Core (review)
import UnliftIO.Async (race)
import UnliftIO.Exception (try)

wsApp ::
  forall r.
  (Eq r, Show r, IsRoute r, EmaStaticSite r) =>
  (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) ->
  LVar (RouteModel r) ->
  EmaWsHandler r ->
  WS.PendingConnection ->
  IO ()
wsApp logger model emaWsHandler pendingConn = do
  conn :: WS.Connection <- WS.acceptRequest pendingConn
  WS.withPingThread conn 30 pass . flip runLoggingT logger $ do
    let log lvl (s :: Text) =
          logWithoutLoc "ema.ws" lvl s
    log LevelInfo "Connected"
    let wsHandler = unEmaWsHandler emaWsHandler conn
        sendRouteHtmlToClient path s = do
          decodeUrlRoute @r s path & \case
            Left err -> do
              log LevelError $ badRouteEncodingMsg err
              liftIO $ WS.sendTextData conn $ emaErrorHtmlResponse $ badRouteEncodingMsg err
            Right Nothing ->
              liftIO $ WS.sendTextData conn $ emaErrorHtmlResponse decodeRouteNothingMsg
            Right (Just r) -> do
              renderCatchingErrors s r >>= \case
                AssetGenerated Html html ->
                  liftIO $ WS.sendTextData conn $ html <> toLazy wsClientHtml
                -- HACK: We expect the websocket client should check for REDIRECT prefix.
                -- Not bothering with JSON response to avoid having to JSON parse every HTML dump.
                AssetStatic _staticPath ->
                  liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText (review (fromPrism_ $ routePrism s) r)
                AssetGenerated Other _s ->
                  liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText (review (fromPrism_ $ routePrism s) r)
              log LevelDebug $ " ~~> " <> show r
        -- @mWatchingRoute@ is the route currently being watched.
        loop mWatchingRoute = do
          -- Listen *until* either we get a new value, or the client requests
          -- to switch to a new route.
          currentModel <- LVar.get model
          race (LVar.listenNext model) (wsHandler currentModel) >>= \case
            Left newModel -> do
              -- The page the user is currently viewing has changed. Send
              -- the new HTML to them.
              sendRouteHtmlToClient mWatchingRoute newModel
              loop mWatchingRoute
            Right mNextRoute -> do
              -- The user clicked on a route link; send them the HTML for
              -- that route this time, ignoring what we are watching
              -- currently (we expect the user to initiate a watch route
              -- request immediately following this).
              sendRouteHtmlToClient mNextRoute =<< LVar.get model
              loop mNextRoute
    -- Wait for the client to send the first request with the initial route.
    mInitialRoute <- wsHandler =<< LVar.get model
    try (loop mInitialRoute) >>= \case
      Right () -> pass
      Left (connExc :: ConnectionException) -> do
        case connExc of
          WS.CloseRequest _ (decodeUtf8 -> reason) ->
            log LevelInfo $ "Closing websocket connection (reason: " <> reason <> ")"
          _ ->
            log LevelError $ "Websocket error: " <> show connExc
