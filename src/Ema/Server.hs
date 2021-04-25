{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | TODO: Refactor this module
module Ema.Server where

import Control.Concurrent.Async (race)
import Control.Exception (catch, try)
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import qualified Data.Text as T
import Ema.Class (Ema (decodeRoute))
import GHC.IO.Unsafe (unsafePerformIO)
import NeatInterpolation (text)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Network.WebSockets (ConnectionException)
import qualified Network.WebSockets as WS

runServerWithWebSocketHotReload ::
  forall model route.
  (Ema model route, Show route) =>
  Int ->
  LVar model ->
  (model -> route -> LByteString) ->
  IO ()
runServerWithWebSocketHotReload port model render = do
  let settings = Warp.setPort port Warp.defaultSettings
  Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions wsApp httpApp
  where
    wsApp pendingConn = do
      conn :: WS.Connection <- WS.acceptRequest pendingConn
      WS.withPingThread conn 30 (pure ()) $ do
        subId <- LVar.addListener model
        let log s = putTextLn $ "[" <> show subId <> "] :: " <> s
        log "ws:connected"
        let askClientForRoute = do
              msg :: Text <- WS.receiveData conn
              pure $
                msg
                  & pathInfoFromWsMsg
                  & routeFromPathInfo
                  & fromMaybe (error "invalid route from ws")
            loop = do
              -- Notice that we @askClientForRoute@ in succession twice here.
              -- The first route will be the route the client intends to observe
              -- for changes on. The second route, *if* it is sent, indicates
              -- that the client wants to *switch* to that route. This proecess
              -- repeats ad infinitum: i.e., the third route is for observing
              -- changes, the fourth route is for switching to, and so on.
              watchingRoute <- askClientForRoute
              log $ "[Watch]: <~~ " <> show watchingRoute
              -- Listen *until* either we get a new value, or the client requests
              -- to switch to a new route.
              race (LVar.listenNext model subId) askClientForRoute >>= \case
                Left newHtml -> do
                  -- The page the user is currently viewing has changed. Send
                  -- the new HTML to them.
                  WS.sendTextData conn $ renderWithEmaHtmlShims newHtml watchingRoute
                  log $ "[Watch]: ~~> " <> show watchingRoute
                  loop
                Right nextRoute -> do
                  -- The user clicked on a route link; send them the HTML for
                  -- that route this time, ignoring what we are watching
                  -- currently (we expect the user to initiate a watch route
                  -- request immediately following this).
                  log $ "[Switch]: <~~ " <> show nextRoute
                  html <- LVar.get model
                  WS.sendTextData conn $ renderWithEmaHtmlShims html nextRoute
                  log $ "[Switch]: ~~> " <> show nextRoute
                  loop
        try loop >>= \case
          Right () -> pure ()
          Left (err :: ConnectionException) -> do
            log $ "ws:error " <> show err
            LVar.removeListener model subId
    httpApp req f = do
      let mr = routeFromPathInfo (Wai.pathInfo req)
      putStrLn $ "[http] " <> show mr
      (status, v) <- case mr of
        Nothing ->
          pure (H.status404, "No route")
        Just r -> do
          val <- LVar.get model
          let html = renderCatchingErrors val r
          pure (H.status200, html <> emaStatusHtml <> wsClientShim)
      f $ Wai.responseLBS status [(H.hContentType, "text/html")] v
    renderWithEmaHtmlShims m r =
      renderCatchingErrors m r <> emaStatusHtml
    renderCatchingErrors m r =
      unsafeCatch (render m r) $ \(err :: SomeException) ->
        encodeUtf8 $
          "<html><head><meta charset=\"UTF-8\"></head><body><h1>Ema App threw an exception</h1><pre style=\"border: 1px solid; padding: 1em 1em 1em 1em;\">"
            <> show @Text err
            <> "</pre><p>Once you fix your code this page will automatically update.</body>"
    routeFromPathInfo =
      decodeRoute @model . fmap (fromString . toString)
    unsafeCatch :: Exception e => a -> (e -> a) -> a
    unsafeCatch x f = unsafePerformIO $ catch (seq x $ pure x) (pure . f)

-- | Return the equivalent of WAI's @pathInfo@, from the raw path string
-- (`document.location.pathname`) the browser sends us.
pathInfoFromWsMsg :: Text -> [Text]
pathInfoFromWsMsg =
  filter (/= "") . T.splitOn "/" . T.drop 1

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
            setHtml(document.documentElement, evt.data);
            // reloadScripts(document.documentElement);
            watchCurrentRoute();
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
