{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ema.Server where

import Control.Monad.Logger
import Data.FileEmbed
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Text qualified as T
import Ema.Asset (
  Asset (AssetGenerated, AssetStatic),
  Format (Html, Other),
 )
import Ema.CLI (Host (unHost))
import Ema.Route.Class (IsRoute (RouteModel, routePrism))
import Ema.Route.Prism (
  checkRoutePrismGivenFilePath,
  fromPrism_,
 )
import Ema.Route.Url (urlToFilePath)
import Ema.Site (EmaSite (siteOutput), EmaStaticSite)
import NeatInterpolation (text)
import Network.HTTP.Types qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.Wai.Middleware.Static qualified as Static
import Network.WebSockets (ConnectionException)
import Network.WebSockets qualified as WS
import Optics.Core (review)
import Text.Printf (printf)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (race)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catch, try)

{- | A handler takes a websocket connection and the current model and then watches
   for websocket messages. It must return a new route to watch (after that, the
   returned route's HTML will be sent back to the client).

  Note that this is usually a long-running thread that waits for the client's
  messages. But you can also use it to implement custom server actions, by handling
  the incoming websocket messages or other IO events in any way you like.

  Also note that whenever the model is updated, the handler action will be
  stopped and then restarted with the new model as argument.
-}
type EmaWsHandler r = WS.Connection -> RouteModel r -> LoggingT IO Text

data EmaServerOptions r = EmaServerOptions
  { emaServerShim :: LByteString
  , emaServerWsHandler :: EmaWsHandler r
  }

defaultEmaWsHandler :: forall r. EmaWsHandler r
defaultEmaWsHandler conn _model = do
  msg :: Text <- liftIO $ WS.receiveData conn
  log LevelDebug $ "<~~ " <> show msg
  pure msg
  where
    log lvl (t :: Text) = logWithoutLoc "ema.ws" lvl t

defaultEmaServerOptions :: forall r. EmaServerOptions r
defaultEmaServerOptions = EmaServerOptions wsClientJS (defaultEmaWsHandler @r)

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
  EmaServerOptions r ->
  Host ->
  Maybe Port ->
  LVar (RouteModel r) ->
  m ()
runServerWithWebSocketHotReload opts host mport model = do
  logger <- askLoggerIO
  let runM = flip runLoggingT logger
      settings =
        Warp.defaultSettings
          & Warp.setHost (fromString . toString . unHost $ host)
      app =
        WaiWs.websocketsOr
          WS.defaultConnectionOptions
          (wsApp logger)
          (httpApp logger)
      banner port = do
        logInfoNS "ema" "==============================================="
        logInfoNS "ema" $ "Ema live server RUNNING: http://" <> unHost host <> ":" <> show port
        logInfoNS "ema" "==============================================="
  liftIO $ warpRunSettings settings mport (runM . banner) app
  where
    enc = routePrism @r
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
    wsApp logger pendingConn = do
      conn :: WS.Connection <- WS.acceptRequest pendingConn
      WS.withPingThread conn 30 pass $
        flip runLoggingT logger $ do
          subId <- LVar.addListener model
          let log lvl (s :: Text) =
                logWithoutLoc (toText @String $ printf "ema.ws.%.2d" subId) lvl s
          log LevelInfo "Connected"
          let wsHandler = emaServerWsHandler opts conn
              sendRouteHtmlToClient path s = do
                decodeUrlRoute s path & \case
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
                        liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText (review (fromPrism_ $ enc s) r)
                      AssetGenerated Other _s ->
                        liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText (review (fromPrism_ $ enc s) r)
                    log LevelDebug $ " ~~> " <> show r
              -- @mWatchingRoute@ is the route currently being watched.
              loop mWatchingRoute = do
                -- Listen *until* either we get a new value, or the client requests
                -- to switch to a new route.
                currentModel <- LVar.get model
                race (LVar.listenNext model subId) (wsHandler currentModel) >>= \case
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
              LVar.removeListener model subId
    httpApp logger req f = do
      flip runLoggingT logger $ do
        val <- LVar.get model
        let pathInfo = Wai.pathInfo req
            path = T.intercalate "/" pathInfo
            mr = decodeUrlRoute val path
        logInfoNS "ema.http" $ "GET " <> path <> " as " <> show mr
        case mr of
          Left err -> do
            logErrorNS "App" $ badRouteEncodingMsg err
            let s = emaErrorHtmlResponse (badRouteEncodingMsg err) <> emaServerShim opts
            liftIO $ f $ Wai.responseLBS H.status500 [(H.hContentType, "text/html")] s
          Right Nothing -> do
            let s = emaErrorHtmlResponse decodeRouteNothingMsg <> emaServerShim opts
            liftIO $ f $ Wai.responseLBS H.status404 [(H.hContentType, "text/html")] s
          Right (Just r) -> do
            renderCatchingErrors val r >>= \case
              AssetStatic staticPath -> do
                let mimeType = Static.getMimeType staticPath
                liftIO $ f $ Wai.responseFile H.status200 [(H.hContentType, mimeType)] staticPath Nothing
              AssetGenerated Html html -> do
                let s = html <> toLazy wsClientHtml <> emaServerShim opts
                liftIO $ f $ Wai.responseLBS H.status200 [(H.hContentType, "text/html")] s
              AssetGenerated Other s -> do
                let mimeType = Static.getMimeType $ review (fromPrism_ $ enc val) r
                liftIO $ f $ Wai.responseLBS H.status200 [(H.hContentType, mimeType)] s
    renderCatchingErrors m r =
      catch (siteOutput (fromPrism_ $ enc m) m r) $ \(err :: SomeException) -> do
        -- Log the error first.
        logErrorNS "App" $ show @Text err
        pure $
          AssetGenerated Html . mkHtmlErrorMsg $
            show @Text err
    -- Decode an URL path into a route
    --
    -- This function is used only in live server. If the route is not
    -- isomoprhic, this returns a Left, with the mismatched encoding.
    decodeUrlRoute :: RouteModel r -> Text -> Either (BadRouteEncoding r) (Maybe r)
    decodeUrlRoute m (urlToFilePath -> s) = do
      case checkRoutePrismGivenFilePath enc m s of
        Left (r, log) -> Left $ BadRouteEncoding s r log
        Right mr -> Right mr

-- | A basic error response for displaying in the browser
emaErrorHtmlResponse :: Text -> LByteString
emaErrorHtmlResponse err =
  mkHtmlErrorMsg err <> toLazy wsClientHtml

mkHtmlErrorMsg :: Text -> LByteString
mkHtmlErrorMsg s =
  encodeUtf8 . T.replace "MESSAGE" s . decodeUtf8 $ $(embedFile "www/ema-error.html")

decodeRouteNothingMsg :: Text
decodeRouteNothingMsg = "Ema: 404 (route decoding returned Nothing)"

data BadRouteEncoding r = BadRouteEncoding
  { _bre_urlFilePath :: FilePath
  , _bre_decodedRoute :: r
  , _bre_checkLog :: [(FilePath, Text)]
  }
  deriving stock (Show)

badRouteEncodingMsg :: (Show r) => BadRouteEncoding r -> Text
badRouteEncodingMsg BadRouteEncoding {..} =
  toText $
    "A route Prism' is unlawful.\n\nThe URL '"
      <> toText _bre_urlFilePath
      <> "' decodes to route '"
      <> show _bre_decodedRoute
      <> "', but it is not isomporphic on any of the allowed candidates: \n\n"
      <> T.intercalate
        "\n\n"
        ( _bre_checkLog <&> \(candidate, log) ->
            "## Candidate '" <> toText candidate <> "':\n" <> log
        )
      <> " \n\nYou should make the relevant routePrism lawful to fix this issue."

wsClientHtml :: ByteString
wsClientHtml = $(embedFile "www/ema-indicator.html")

wsClientJSShim :: Text
wsClientJSShim = decodeUtf8 $(embedFile "www/ema-shim.js")

-- Browser-side JavaScript code for interacting with the Haskell server
wsClientJS :: LByteString
wsClientJS =
  encodeUtf8
    [text|
        <script type="module" src="https://cdn.jsdelivr.net/npm/morphdom@2.6.1/dist/morphdom-umd.min.js"></script>

        <script type="module">
        ${wsClientJSShim}
        
        window.onpageshow = function () { init(false) };
        </script>
    |]
