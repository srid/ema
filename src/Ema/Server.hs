{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Server where

import Control.Concurrent.Async (race)
import Control.Exception (catch, try)
import Control.Monad.Logger
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import qualified Data.Text as T
import Ema.Route (FileRoute (..))
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

runServerWithWebSocketHotReload ::
  forall model route m.
  ( FileRoute route,
    Show route,
    MonadIO m,
    MonadUnliftIO m,
    MonadLoggerIO m
  ) =>
  Int ->
  LVar model ->
  (model -> route -> Either FilePath LByteString) ->
  m ()
runServerWithWebSocketHotReload port model render = do
  let settings = Warp.setPort port Warp.defaultSettings
  logger <- askLoggerIO

  logInfoN $ "Launching Ema at http://localhost:" <> show port
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
                  let r =
                        msg
                          & pathInfoFromWsMsg
                          & routeFromPathInfo
                          & fromMaybe (error "invalid route from ws")
                  log LevelDebug $ "<~~ " <> show r
                  pure r
                sendRouteHtmlToClient r s = do
                  case renderWithEmaHtmlShims logger s r of
                    Left staticPath ->
                      -- HACK: Websocket client should check for REDIRECT prefix.
                      -- Not bothering with JSON to avoid having to JSON parse every HTML dump.
                      liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText staticPath
                    Right html ->
                      liftIO $ WS.sendTextData conn html
                  log LevelDebug $ " ~~> " <> show r
                loop = flip runLoggingT logger $ do
                  -- Notice that we @askClientForRoute@ in succession twice here.
                  -- The first route will be the route the client intends to observe
                  -- for changes on. The second route, *if* it is sent, indicates
                  -- that the client wants to *switch* to that route. This proecess
                  -- repeats ad infinitum: i.e., the third route is for observing
                  -- changes, the fourth route is for switching to, and so on.
                  watchingRoute <- askClientForRoute
                  -- Listen *until* either we get a new value, or the client requests
                  -- to switch to a new route.
                  liftIO $ do
                    race (LVar.listenNext model subId) (runLoggingT askClientForRoute logger) >>= \res -> flip runLoggingT logger $ case res of
                      Left newModel -> do
                        -- The page the user is currently viewing has changed. Send
                        -- the new HTML to them.
                        sendRouteHtmlToClient watchingRoute newModel
                        lift loop
                      Right nextRoute -> do
                        -- The user clicked on a route link; send them the HTML for
                        -- that route this time, ignoring what we are watching
                        -- currently (we expect the user to initiate a watch route
                        -- request immediately following this).
                        sendRouteHtmlToClient nextRoute =<< LVar.get model
                        lift loop
            liftIO (try loop) >>= \case
              Right () -> pure ()
              Left (err :: ConnectionException) -> do
                log LevelError $ "Websocket error: " <> show err
                LVar.removeListener model subId
    assetsMiddleware =
      Static.static
    httpApp logger req f = do
      flip runLoggingT logger $ do
        let path = Wai.pathInfo req
            mr = routeFromPathInfo path
        logInfoNS "HTTP" $ show path <> " as " <> show mr
        case mr of
          Nothing ->
            liftIO $ f $ Wai.responseLBS H.status404 [(H.hContentType, "text/plain")] "No route"
          Just r -> do
            val <- LVar.get model
            case renderCatchingErrors logger val r of
              Left staticPath -> do
                let mimeType = Static.getMimeType staticPath
                liftIO $ f $ Wai.responseFile H.status200 [(H.hContentType, mimeType)] staticPath Nothing
              Right html -> do
                let s = html <> emaStatusHtml <> wsClientShim
                liftIO $ f $ Wai.responseLBS H.status200 [(H.hContentType, "text/html")] s
    renderWithEmaHtmlShims logger m r =
      renderCatchingErrors logger m r <&> (<> emaStatusHtml)
    renderCatchingErrors logger m r =
      unsafeCatch (render m r) $ \(err :: SomeException) ->
        unsafePerformIO $ do
          -- Log the error first.
          flip runLoggingT logger $ logErrorNS "App" $ show @Text err
          pure $
            Right $
              encodeUtf8 $
                "<html><head><meta charset=\"UTF-8\"></head><body><h1>Ema App threw an exception</h1><pre style=\"border: 1px solid; padding: 1em 1em 1em 1em;\">"
                  <> show @Text err
                  <> "</pre><p>Once you fix your code this page will automatically update.</body>"
    routeFromPathInfo =
      decodeUrlRoute @route . T.intercalate "/"
    -- TODO: It would be good have this also get us the stack trace.
    unsafeCatch :: Exception e => a -> (e -> a) -> a
    unsafeCatch x f = unsafePerformIO $ catch (seq x $ pure x) (pure . f)

-- | Return the equivalent of WAI's @pathInfo@, from the raw path string
-- (`document.location.pathname`) the browser sends us.
pathInfoFromWsMsg :: Text -> [Text]
pathInfoFromWsMsg =
  filter (/= "") . T.splitOn "/" . T.drop 1

-- | Decode a URL path into a route
--
-- This function is used only in live server.
decodeUrlRoute :: FileRoute route => Text -> Maybe route
decodeUrlRoute (toString -> s) =
  decodeFileRoute s
    <|> decodeFileRoute (s <> ".html")
    <|> decodeFileRoute (s </> "index.html")

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
        };

        // FIXME: Can't make this work with tailwind shim
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

        // WebSocket logic: watching for server changes & route switching
        function init() {
          console.log("ema: Opening ws conn");
          window.connecting();
          var ws = new WebSocket("ws://" + window.location.host);

          // Call this, then the server will send update *once*. Call again for
          // continous monitoring.
          function watchCurrentRoute() {
            console.log(`ema: ⏿ Observing changes to $${document.location.pathname}`);
            ws.send(document.location.pathname);
          };

          function switchRoute(path) {
             console.log(`ema: → Switching to $${path}`);
             ws.send(path);
          }

          function handleRouteClicks(e) {
              const origin = e.target.closest("a");
              if (origin) {
                if (window.location.host === origin.host) {
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
            // window.connected();
            window.hideIndicator();
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
            setTimeout(init, 400);
          };

          ws.onmessage = evt => {
            console.log("ema: ✍ Patching DOM")
            if evt.data.startsWith("REDIRECT ") {
              document.location.href = evt.data.str.slice("REDIRECT ".length);
            } else {
              setHtml(document.documentElement, evt.data);
              // reloadScripts(document.documentElement);
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
        
        window.onpageshow = init;
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
            class="bg-green-600 w-3 h-3 rounded-full flex-none"
            id="ema-connected"
          ></div>
          <div
            hidden
            class="
              animate-spin bg-gradient-to-r from-blue-300 to-blue-600
              w-3 h-3 rounded-full flex-none
            "
            id="ema-reloading"
          ></div>
          <div
            hidden
            class="bg-yellow-500 w-3 h-3 rounded-full flex-none"
            id="ema-connecting"
          >
            <div
              class="animate-ping bg-yellow-500 w-3 h-3 rounded-full flex-none"
            ></div>
          </div>
          <div
            hidden
            class="bg-red-500 w-3 h-3 rounded-full flex-none"
            id="ema-disconnected"
          ></div>
          <p class="whitespace-nowrap" id="ema-message"></p>
        </div>
      </div>
  |]
