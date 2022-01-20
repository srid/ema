{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Server where

import Control.Concurrent.Async (race)
import Control.Exception (catch, try)
import Control.Monad.Logger
import Data.Default
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import qualified Data.Text as T
import Ema.Asset
import Ema.Class (Ema (..))
import GHC.IO.Unsafe (unsafePerformIO)
import NeatInterpolation (text)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.Wai.Middleware.Static as Static
import Network.WebSockets (ConnectionException)
import qualified Network.WebSockets as WS
import System.FilePath ((</>))
import Text.Printf (printf)
import UnliftIO (MonadUnliftIO)

-- | Host string to start the server on.
newtype Host = Host {unHost :: Text}
  deriving newtype (Eq, Show, Ord, IsString)

-- | Port number to bind the server on.
newtype Port = Port {unPort :: Int}
  deriving newtype (Eq, Show, Ord, Num, Read)

instance Default Host where
  def = "127.0.0.1"

instance Default Port where
  def = 8000

runServerWithWebSocketHotReload ::
  forall model route m.
  ( Ema model route,
    Show route,
    MonadIO m,
    MonadUnliftIO m,
    MonadLoggerIO m
  ) =>
  Host ->
  Port ->
  LVar model ->
  (model -> route -> Asset LByteString) ->
  m ()
runServerWithWebSocketHotReload host port model render = do
  let settings =
        Warp.defaultSettings
          & Warp.setPort (unPort port)
          & Warp.setHost (fromString . toString . unHost $ host)
  logger <- askLoggerIO

  logInfoN "============================================"
  logInfoN $ "Running live server at http://" <> unHost host <> ":" <> show port
  logInfoN "============================================"
  liftIO $
    Warp.runSettings settings $
      assetsMiddleware $
        WaiWs.websocketsOr
          WS.defaultConnectionOptions
          (flip runLoggingT logger . wsApp)
          (httpApp logger)
  where
    wsApp pendingConn = do
      conn :: WS.Connection <- lift $ WS.acceptRequest pendingConn
      logger <- askLoggerIO
      lift $
        WS.withPingThread conn 30 (pure ()) $
          flip runLoggingT logger $ do
            subId <- LVar.addListener model
            let log lvl (s :: Text) =
                  logWithoutLoc (toText @String $ printf "WS.Client.%.2d" subId) lvl s
            log LevelInfo "Connected"
            let askClientForRoute = do
                  msg :: Text <- liftIO $ WS.receiveData conn
                  -- TODO: Let non-html routes pass through.
                  let pathInfo = pathInfoFromWsMsg msg
                  log LevelDebug $ "<~~ " <> show pathInfo
                  pure pathInfo
                decodeRouteWithCurrentModel pathInfo = do
                  val <- LVar.get model
                  pure $ routeFromPathInfo val pathInfo
                sendRouteHtmlToClient pathInfo s = do
                  decodeRouteWithCurrentModel pathInfo >>= \case
                    Nothing ->
                      liftIO $ WS.sendTextData conn $ emaErrorHtmlResponse decodeRouteNothingMsg
                    Just r -> do
                      case renderCatchingErrors logger s r of
                        AssetStatic staticPath ->
                          -- HACK: Websocket client should check for REDIRECT prefix.
                          -- Not bothering with JSON to avoid having to JSON parse every HTML dump.
                          liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText staticPath
                        AssetGenerated Html html ->
                          liftIO $ WS.sendTextData conn $ html <> emaStatusHtml
                        AssetGenerated Other _s ->
                          -- HACK: Websocket client should check for REDIRECT prefix.
                          -- Not bothering with JSON to avoid having to JSON parse every HTML dump.
                          liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText (encodeRoute s r)
                      log LevelDebug $ " ~~> " <> show r
                loop = flip runLoggingT logger $ do
                  -- Notice that we @askClientForRoute@ in succession twice here.
                  -- The first route will be the route the client intends to observe
                  -- for changes on. The second route, *if* it is sent, indicates
                  -- that the client wants to *switch* to that route. This proecess
                  -- repeats ad infinitum: i.e., the third route is for observing
                  -- changes, the fourth route is for switching to, and so on.
                  mWatchingRoute <- askClientForRoute
                  -- Listen *until* either we get a new value, or the client requests
                  -- to switch to a new route.
                  liftIO $ do
                    race (LVar.listenNext model subId) (runLoggingT askClientForRoute logger) >>= \res -> flip runLoggingT logger $ case res of
                      Left newModel -> do
                        -- The page the user is currently viewing has changed. Send
                        -- the new HTML to them.
                        sendRouteHtmlToClient mWatchingRoute newModel
                        lift loop
                      Right mNextRoute -> do
                        -- The user clicked on a route link; send them the HTML for
                        -- that route this time, ignoring what we are watching
                        -- currently (we expect the user to initiate a watch route
                        -- request immediately following this).
                        sendRouteHtmlToClient mNextRoute =<< LVar.get model
                        lift loop
            liftIO (try loop) >>= \case
              Right () -> pure ()
              Left (connExc :: ConnectionException) -> do
                case connExc of
                  WS.CloseRequest _ (decodeUtf8 -> reason) ->
                    log LevelInfo $ "Closing websocket connection (reason: " <> reason <> ")"
                  _ ->
                    log LevelError $ "Websocket error: " <> show connExc
                LVar.removeListener model subId
    assetsMiddleware =
      Static.static
    httpApp logger req f = do
      flip runLoggingT logger $ do
        val <- LVar.get model
        let path = Wai.pathInfo req
            mr = routeFromPathInfo val path
        logInfoNS "HTTP" $ show path <> " as " <> show mr
        case mr of
          Nothing ->
            liftIO $ f $ Wai.responseLBS H.status404 [(H.hContentType, "text/plain")] $ encodeUtf8 decodeRouteNothingMsg
          Just r -> do
            case renderCatchingErrors logger val r of
              AssetStatic staticPath -> do
                let mimeType = Static.getMimeType staticPath
                liftIO $ f $ Wai.responseFile H.status200 [(H.hContentType, mimeType)] staticPath Nothing
              AssetGenerated Html html -> do
                let s = html <> emaStatusHtml <> wsClientShim
                liftIO $ f $ Wai.responseLBS H.status200 [(H.hContentType, "text/html")] s
              AssetGenerated Other s -> do
                let mimeType = Static.getMimeType $ encodeRoute val r
                liftIO $ f $ Wai.responseLBS H.status200 [(H.hContentType, mimeType)] s
    renderCatchingErrors logger m r =
      unsafeCatch (render m r) $ \(err :: SomeException) ->
        unsafePerformIO $ do
          -- Log the error first.
          flip runLoggingT logger $ logErrorNS "App" $ show @Text err
          pure $
            AssetGenerated Html . emaErrorHtml $
              show @Text err
    routeFromPathInfo m =
      decodeUrlRoute m . T.intercalate "/"
    -- TODO: It would be good have this also get us the stack trace.
    unsafeCatch :: Exception e => a -> (e -> a) -> a
    unsafeCatch x f = unsafePerformIO $ catch (seq x $ pure x) (pure . f)

-- | A basic error response for displaying in the browser
emaErrorHtmlResponse :: Text -> LByteString
emaErrorHtmlResponse err =
  emaErrorHtml err <> emaStatusHtml

emaErrorHtml :: Text -> LByteString
emaErrorHtml s =
  encodeUtf8 $
    "<html><head><meta charset=\"UTF-8\"></head><body><h1>Ema App threw an exception</h1><pre style=\"border: 1px solid; padding: 1em 1em 1em 1em;\">"
      <> s
      <> "</pre><p>Once you fix the source of the error, this page will automatically refresh.</body>"

-- | Return the equivalent of WAI's @pathInfo@, from the raw path string
-- (`document.location.pathname`) the browser sends us.
pathInfoFromWsMsg :: Text -> [Text]
pathInfoFromWsMsg =
  filter (/= "") . T.splitOn "/" . T.drop 1

-- | Decode a URL path into a route
--
-- This function is used only in live server.
decodeUrlRoute :: forall model route. Ema model route => model -> Text -> Maybe route
decodeUrlRoute model (toString -> s) = do
  decodeRoute @model @route model s
    <|> decodeRoute @model @route model (s <> ".html")
    <|> decodeRoute @model @route model (s </> "index.html")

decodeRouteNothingMsg :: Text
decodeRouteNothingMsg = "Ema: 404 (decodeRoute returned Nothing)"

-- Browser-side JavaScript code for interacting with the Haskell server
wsClientShim :: LByteString
wsClientShim =
  encodeUtf8
    [text|
        <script type="module" src="https://cdn.jsdelivr.net/npm/morphdom@2.6.1/dist/morphdom-umd.min.js"></script>

        <script type="module">

        function htmlToElem(html) {
          let temp = document.createElement('template');
          html = html.trim(); // Never return a space text node as a result
          temp.innerHTML = html;
          return temp.content.firstChild;
        };

        // Unlike setInnerHtml, this patches the Dom in place
        function setHtml(elm, html) {
          var htmlElem = htmlToElem(html);
          morphdom(elm, html);
          // Re-add <script> tags, because just DOM diff applying is not enough.
          reloadScripts(elm);
        };

        // FIXME: This doesn't reliably work across all JS.
        // See also the HACK below in one of the invocations.
        function reloadScripts(elm) {
          Array.from(elm.querySelectorAll("script")).forEach(oldScript => {
            const newScript = document.createElement("script");
            Array.from(oldScript.attributes)
              .forEach(attr => newScript.setAttribute(attr.name, attr.value));
            newScript.appendChild(document.createTextNode(oldScript.innerHTML));
            oldScript.parentNode.replaceChild(newScript, oldScript);
          });
        };

        // Ema Status indicator
        const messages = {
          connected: "Connected",
          reloading: "Reloading",
          connecting: "Connecting to the server",
          disconnected: "Disconnected - try reloading the window"
        };
        function setIndicators(connected, reloading, connecting, disconnected) {
          const is = { connected, reloading, connecting, disconnected }

          for (const i in is) {
            document.getElementById(`ema-$${i}`).style.display =
              is[i] ? "block" : "none"
            if(is[i])
              document.getElementById('ema-message').innerText = messages[i]
          };
          document.getElementById("ema-indicator").style.display = "block";
        };
        window.connected    = () => setIndicators(true,  false, false, false)
        window.reloading    = () => setIndicators(false, true,  false, false)
        window.connecting   = () => setIndicators(false, false, true,  false)
        window.disconnected = () => setIndicators(false, false, false, true)
        window.hideIndicator = () => {
          document.getElementById("ema-indicator").style.display = "none";
        };

        // Base URL path - for when the ema site isn't served at "/"
        const baseHref = document.getElementsByTagName("base")[0]?.href;
        const basePath = baseHref ? new URL(baseHref).pathname : "/";

        // Use TLS for websocket iff the current page is also served with TLS
        const wsProto = window.location.protocol === "https:" ? "wss://" : "ws://";
        const wsUrl = wsProto + window.location.host + basePath;

        // WebSocket logic: watching for server changes & route switching
        function init(reconnecting) {
          // The route current DOM is displaying
          let routeVisible = document.location.pathname;

          const verb = reconnecting ? "Reopening" : "Opening";
          console.log(`ema: $${verb} conn $${wsUrl} ...`);
          window.connecting();
          let ws = new WebSocket(wsUrl);

          function sendObservePath(path) {
            const relPath = path.startsWith(basePath) ? path.slice(basePath.length - 1) : path;
            console.debug(`ema: requesting $${relPath}`);
            ws.send(relPath);
          }

          // Call this, then the server will send update *once*. Call again for
          // continous monitoring.
          function watchCurrentRoute() {
            console.log(`ema: ⏿ Observing changes to $${document.location.pathname}`);
            sendObservePath(document.location.pathname);
          };

          function switchRoute(path) {
             console.log(`ema: → Switching to $${path}`);
             sendObservePath(path);
          }

          function handleRouteClicks(e) {
              const origin = e.target.closest("a");
              if (origin) {
                if (window.location.host === origin.host && origin.getAttribute("target") != "_blank") {
                  window.history.pushState({}, "", origin.pathname);
                  switchRoute(origin.pathname);
                  e.preventDefault();
                };
              }
            };
          // Intercept route click events, and ask server for its HTML whilst
          // managing history state.
          window.addEventListener(`click`, handleRouteClicks);

          ws.onopen = () => {
            console.log(`ema: ... connected!`);
            // window.connected();
            window.hideIndicator();
            if (!reconnecting) {
              // HACK: We have to reload <script>'s here on initial page load
              // here, so as to make Twind continue to function on the *next*
              // route change. This is not a problem with *subsequent* (ie. 2nd
              // or latter) route clicks, because those have already called
              // reloadScripts at least once.
              reloadScripts(document.documentElement);
            };
            watchCurrentRoute();
          };

          ws.onclose = () => {
            console.log("ema: reconnecting ..");
            window.removeEventListener(`click`, handleRouteClicks);
            window.reloading();
            // Reconnect after as small a time is possible, then retry again. 
            // ghcid can take 1s or more to reboot. So ideally we need an
            // exponential retry logic.
            // 
            // Note that a slow delay (200ms) may often cause websocket
            // connection error (ghcid hasn't rebooted yet), which cannot be
            // avoided as it is impossible to trap this error and handle it.
            // You'll see a big ugly error in the console.
            setTimeout(function () {init(true);}, 400);
          };

          ws.onmessage = evt => {
            if (evt.data.startsWith("REDIRECT ")) {
              console.log("ema: redirect");
              document.location.href = evt.data.slice("REDIRECT ".length);
            } else {
              console.log("ema: ✍ Patching DOM");
              setHtml(document.documentElement, evt.data);
              if (routeVisible != document.location.pathname) {
                // This is a new route switch; scroll up.
                window.scrollTo({ top: 0});
                routeVisible = document.location.pathname;
              } 
              watchCurrentRoute();
            };
          };
          window.onbeforeunload = evt => { ws.close(); };
          window.onpagehide = evt => { ws.close(); };

          // When the user clicks the back button, resume watching the URL in
          // the addressback, which has the effect of loading it immediately.
          window.onpopstate = function(e) {
            watchCurrentRoute();
          };
        };
        
        window.onpageshow = function () { init(false) };
        </script>
    |]

emaStatusHtml :: LByteString
emaStatusHtml =
  encodeUtf8
    [text|
      <div class="absolute top-0 left-0 p-2" style="display: none;" id="ema-indicator">
        <div
          class="
            flex overflow-hidden items-center p-2 text-xs gap-2
            h-8 border-2 border-gray-200 bg-white rounded-full shadow-lg
            transition-[width,height] duration-500 ease-in-out w-8 hover:w-full
          "
          id="ema-status"
          title="Ema Status"
        >
          <div
            hidden
            class="flex-none w-3 h-3 bg-green-600 rounded-full"
            id="ema-connected"
          ></div>
          <div
            hidden
            class="flex-none w-3 h-3 rounded-full animate-spin bg-gradient-to-r from-blue-300 to-blue-600"
            id="ema-reloading"
          ></div>
          <div
            hidden
            class="flex-none w-3 h-3 bg-yellow-500 rounded-full"
            id="ema-connecting"
          >
            <div
              class="flex-none w-3 h-3 bg-yellow-500 rounded-full animate-ping"
            ></div>
          </div>
          <div
            hidden
            class="flex-none w-3 h-3 bg-red-500 rounded-full"
            id="ema-disconnected"
          ></div>
          <p class="whitespace-nowrap" id="ema-message"></p>
        </div>
      </div>
  |]
