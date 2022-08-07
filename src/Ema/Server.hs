{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ema.Server where

import Control.Concurrent.Async (race)
import Control.Exception (try)
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
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catch)

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
  Host ->
  Maybe Port ->
  LVar (RouteModel r) ->
  m ()
runServerWithWebSocketHotReload host mport model = do
  logger <- askLoggerIO
  let runM = flip runLoggingT logger
      settings =
        Warp.defaultSettings
          & Warp.setHost (fromString . toString . unHost $ host)
      app =
        WaiWs.websocketsOr
          WS.defaultConnectionOptions
          (runM . wsApp)
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
    wsApp pendingConn = do
      conn :: WS.Connection <- lift $ WS.acceptRequest pendingConn
      logger <- askLoggerIO
      lift $
        WS.withPingThread conn 30 pass $
          flip runLoggingT logger $ do
            subId <- LVar.addListener model
            let log lvl (s :: Text) =
                  logWithoutLoc (toText @String $ printf "ema.ws.%.2d" subId) lvl s
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
                    Left err -> do
                      log LevelError $ badRouteEncodingMsg err
                      liftIO $ WS.sendTextData conn $ emaErrorHtmlResponse $ badRouteEncodingMsg err
                    Right Nothing ->
                      liftIO $ WS.sendTextData conn $ emaErrorHtmlResponse decodeRouteNothingMsg
                    Right (Just r) -> do
                      renderCatchingErrors s r >>= \case
                        AssetStatic staticPath ->
                          -- HACK: Websocket client should check for REDIRECT prefix.
                          -- Not bothering with JSON to avoid having to JSON parse every HTML dump.
                          liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText staticPath
                        AssetGenerated Html html ->
                          liftIO $ WS.sendTextData conn $ html <> toLazy wsClientHtml
                        AssetGenerated Other _s ->
                          -- HACK: Websocket client should check for REDIRECT prefix.
                          -- Not bothering with JSON to avoid having to JSON parse every HTML dump.
                          liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText (review (fromPrism_ $ enc s) r)
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
        let path = Wai.pathInfo req
            mr = routeFromPathInfo val path
        logInfoNS "ema.http" $ "GET " <> ("/" <> T.intercalate "/" path) <> " as " <> show mr
        case mr of
          Left err -> do
            logErrorNS "App" $ badRouteEncodingMsg err
            let s = emaErrorHtmlResponse (badRouteEncodingMsg err) <> wsClientJS
            liftIO $ f $ Wai.responseLBS H.status500 [(H.hContentType, "text/html")] s
          Right Nothing -> do
            let s = emaErrorHtmlResponse decodeRouteNothingMsg <> wsClientJS
            liftIO $ f $ Wai.responseLBS H.status404 [(H.hContentType, "text/html")] s
          Right (Just r) -> do
            renderCatchingErrors val r >>= \case
              AssetStatic staticPath -> do
                let mimeType = Static.getMimeType staticPath
                liftIO $ f $ Wai.responseFile H.status200 [(H.hContentType, mimeType)] staticPath Nothing
              AssetGenerated Html html -> do
                let s = html <> toLazy wsClientHtml <> wsClientJS
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
    routeFromPathInfo m =
      decodeUrlRoute m . T.intercalate "/"
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

-- TODO: Make this pretty
mkHtmlErrorMsg :: Text -> LByteString
mkHtmlErrorMsg s =
  encodeUtf8 $
    "<html><head><meta charset=\"UTF-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /></head><body class=\"overflow-y: scroll;\"><h1>Ema App threw an exception</h1><pre style=\"font-family: monospace; border: 1px solid; padding: 1em 1em 1em 1em; overflow-wrap: anywhere;\">"
      <> s
      <> "</pre><p>Once you fix the source of the error, this page will automatically refresh.</body>"

{- | Return the equivalent of WAI's @pathInfo@, from the raw path string
 (`document.location.pathname`) the browser sends us.
-}
pathInfoFromWsMsg :: Text -> [Text]
pathInfoFromWsMsg =
  filter (/= "") . T.splitOn "/" . T.drop 1

decodeRouteNothingMsg :: Text
decodeRouteNothingMsg = "Ema: 404 (route decoding returned Nothing)"

data BadRouteEncoding r = BadRouteEncoding
  { _bre_urlFilePath :: FilePath
  , _bre_decodedRoute :: r
  , _bre_checkLog :: [(FilePath, Text)]
  }
  deriving stock (Show)

badRouteEncodingMsg :: Show r => BadRouteEncoding r -> Text
badRouteEncodingMsg BadRouteEncoding {..} =
  toText $
    "A route Prism' is unlawful.\n\nThe URL '" <> toText _bre_urlFilePath
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

-- Browser-side JavaScript code for interacting with the Haskell server
wsClientJS :: LByteString
wsClientJS =
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

          function switchRoute(path, hash = "") {
             console.log(`ema: → Switching to $${path + hash}`);
             window.history.pushState({}, "", path + hash);
             sendObservePath(path);
          }

          function scrollToAnchor(hash) {
            console.log(`ema: Scroll to $${hash}`)
            var el = document.querySelector(hash);
            if (el !== null) {
              el.scrollIntoView({ behavior: 'smooth' });
            }
          };

          function handleRouteClicks(e) {
              const origin = e.target.closest("a");
              if (origin) {
                if (window.location.host === origin.host && origin.getAttribute("target") != "_blank") {
                  if (origin.getAttribute("href").startsWith("#")) {
                    // Switching to local anchor
                    window.history.pushState({}, "", window.location.pathname + origin.hash);
                    scrollToAnchor(window.location.hash);
                    e.preventDefault();
                  }else{
                    // Switching to another route
                    switchRoute(origin.pathname, origin.hash);
                    e.preventDefault();
                  }
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
              if (window.location.hash) {
                scrollToAnchor(window.location.hash);
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

          // API for user invocations 
          window.ema = {
            switchRoute: switchRoute
          };
        };
        
        window.onpageshow = function () { init(false) };
        </script>
    |]
