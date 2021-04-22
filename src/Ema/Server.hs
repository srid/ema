{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | TODO: Refactor this module
module Ema.Server where

import Control.Concurrent.Async (race)
import Control.Exception (try)
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import qualified Data.Text as T
import Ema.Route (IsRoute (..))
import NeatInterpolation (text)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Network.WebSockets (ConnectionException)
import qualified Network.WebSockets as WS

runServerWithWebSocketHotReload ::
  forall model route.
  (Show route, IsRoute route) =>
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
        log "ws connected"
        let loop = do
              msg <- WS.receiveData conn
              let r :: route =
                    msg
                      & pathInfoFromWsMsg
                      & routeFromPathInfo
                      & fromMaybe (error "invalid route from ws")
              log $ "!!!: Browser requests next HTML for: " <> show r
              eVal <- race (LVar.listenNext model subId) $ do
                msg2 :: Text <- WS.receiveData conn
                pure $
                  msg2
                    & pathInfoFromWsMsg
                    & routeFromPathInfo
                    & fromMaybe (error "invalid route from ws")
              case eVal of
                Left val -> do
                  WS.sendTextData conn $ routeHtml val r
                  log $ "!!!: Sent HTML for " <> show r
                  loop
                Right routeToSwitch -> do
                  log $ "~~>: request to switch"
                  val <- LVar.get model
                  WS.sendTextData conn $ routeHtml val routeToSwitch
                  log $ "~~>: Sent HTML for " <> show routeToSwitch
                  loop
        try loop >>= \case
          Right () -> pure ()
          Left (err :: ConnectionException) -> do
            log $ "ws:error " <> show err
            LVar.removeListener model subId
    httpApp req f = do
      (status, v) <- case routeFromPathInfo (Wai.pathInfo req) of
        Nothing ->
          pure (H.status404, "No route")
        Just r -> do
          val <- LVar.get model
          pure (H.status200, routeHtml val r)
      f $ Wai.responseLBS status [(H.hContentType, "text/html")] v
    routeFromPathInfo =
      fromSlug . fmap (fromString . toString)
    routeHtml :: model -> route -> LByteString
    routeHtml m r = do
      render m r <> emaStatusHtml <> wsClientShim

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
        <script type="module">
        // https://stackoverflow.com/a/47614491/55246
        function setInnerHtml(elm, html) {
          elm.innerHTML = html;
          Array.from(elm.querySelectorAll("script")).forEach(oldScript => {
            const newScript = document.createElement("script");
            Array.from(oldScript.attributes)
              .forEach(attr => newScript.setAttribute(attr.name, attr.value));
            newScript.appendChild(document.createTextNode(oldScript.innerHTML));
            oldScript.parentNode.replaceChild(newScript, oldScript);
          });
        }

        // Unused, right now.
        function refreshPage() {
          // The setTimeout is necessary, otherwise reload will hang forever (at
          // least on Brave browser)
          // 
          // The delayedRefresh trick (5000 and 2000) is for cases when the
          // server hasn't reloaded fast enough, but the browser hangs forever
          // in reload refresh state.
          //
          // FIXME: This is not enough. Cancel and retry the reload, as it is
          // normal to have longer sessions of ghcid in error state while the
          // user fixes their code.
          setTimeout(function() {
            window.location.reload();
          }, 5000);
          setTimeout(function() {
            window.location.reload();
          }, 2000);
          setTimeout(function() {
            window.location.reload();
          }, 100);
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

        // WebSocket logic
        function init() {
          console.log("ema: Opening ws conn");
          window.connecting();
          var ws = new WebSocket("ws://" + window.location.host);

          // Call this, then the server will send update *once*. Call again for
          // continous monitoring.
          function watchRoute() {
            ws.send(document.location.pathname);
          };

          ws.onopen = () => {
            // window.connected();
            window.hideIndicator();
            console.log("ema: Observing server for changes");
            watchRoute();
          };
          ws.onclose = () => {
            console.log("ema: closed; reconnecting ..");
            window.reloading();
            setTimeout(init, 1000);
            // refreshPage();
          };
          ws.onmessage = evt => {
            console.log("ema: Resetting HTML body")
            setInnerHtml(document.documentElement, evt.data);
            document.body.addEventListener(`click`, e => {
              const origin = e.target.closest("a");
              if (origin) {
                console.log(`You clicked ${origin.href}`);
                if (window.location.host === origin.host) {
                  // Hey server, cancel my previous watch - and load this
                  console.log(`hey server: ${origin.pathname} vs ${document.location.pathname}`);
                  document.body.classList.add("opacity-20");
                  window.history.pushState({}, "", origin.pathname);
                  ws.send(origin.pathname);
                  e.preventDefault();
                };
              }
            });
            watchRoute();
          };
          window.onbeforeunload = evt => { ws.close(); };
          window.onpagehide = evt => { ws.close(); };

          window.onpopstate = function(e) {
            document.body.classList.add("opacity-20");
            ws.send(document.location.pathname);
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
