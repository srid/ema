{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | TODO: Refactor this module
module Ema.App where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM.TVar (swapTVar)
import Control.Exception (try)
import qualified Data.Text as T
import Ema.Route (IsRoute (..))
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)
import NeatInterpolation (text)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Network.WebSockets (ConnectionException)
import qualified Network.WebSockets as WS

-- A mutable variable with change notification
--
-- one subscriber only (because of TMVar)
--
-- TODO: Rename to @TVarO@ and put in a module
-- TODO: refactor? https://hackage.haskell.org/package/websockets-0.7.2.0/docs/Network-WebSockets-Util-PubSub.html
data Changing a = Changing
  { changingTVar :: TVar a,
    changingEvent :: TMVar ()
  }

data Ema s r = Ema
  { emaModel :: Changing s,
    emaRenderRoute :: s -> r -> LByteString
  }

-- | Pure version of @runEma@ that is independent of the world outside the program.
--
-- Due to purity, there is no model that varies over time. The render function
-- can use any value from the closure.
runEmaPure ::
  forall r.
  (IsRoute r, Show r) =>
  -- | How to render a route
  (r -> LByteString) ->
  IO ()
runEmaPure render = do
  model <- Changing <$> newTVarIO () <*> newEmptyTMVarIO
  let ema = Ema model (const render)
  runEma ema

-- | Run Ema live server
--
-- Continually observe the world, with a concomitant incremental update of the
-- model so as to hot-reload the browser view.
runEma ::
  forall model route.
  (Show route, IsRoute route) =>
  Ema model route ->
  IO ()
runEma ema = do
  -- TODO: Use a logging library, in place of managing buffering and using putStrLn
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "Launching Ema"
  let settings = Warp.setPort 8000 Warp.defaultSettings
  currentWsConn :: TVar (Maybe (route, WS.Connection)) <- newTVarIO Nothing
  -- bracket helps clean up after ghcid reboot
  race_ (notifyWsClient currentWsConn) $
    Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions (wsApp currentWsConn) httpApp
  where
    notifyWsClient currentWsConn = do
      let log s = putTextLn $ " :: " <> s
          ch = changingEvent $ emaModel ema
      forever $ do
        -- FIXME: 100% cpu usage if currentWsConn is nothing?
        -- Just spawn this thread on demand, when conn comes in.
        readTVarIO currentWsConn >>= \case
          Nothing -> pure ()
          Just (r, conn) -> do
            atomically $ takeTMVar ch
            log $ "Sending new HTML for: " <> show r
            s <- routeBS r
            try (WS.sendTextData conn s) >>= \case
              Right () -> pure ()
              Left (err :: ConnectionException) ->
                log $ "notify:: " <> show err

    wsApp currentWsConn pendingConn = do
      let log s = putTextLn $ " :: " <> s
      log "ws connected"
      conn :: WS.Connection <- WS.acceptRequest pendingConn
      WS.withPingThread conn 30 (pure ()) $ do
        watchingRoute :: Text <- WS.receiveData conn
        let pathInfo = filter (/= "") $ T.splitOn "/" $ T.drop 1 watchingRoute
            r :: route = fromMaybe (error "invalid route from ws") $ fromSlug $ fromString . toString <$> pathInfo
        log $ "Browser loaded: " <> show pathInfo <> " => " <> show r
        -- NOTE: only one active client is supported for hot-reload; not
        -- bothering with client management for now, because that will require
        -- broadcast TChan.
        void $ atomically $ swapTVar currentWsConn $ Just (r, conn)
        let loop = do
              try (WS.receiveDataMessage conn) >>= \case
                Right (_ :: WS.DataMessage) -> loop
                Left (err :: ConnectionException) -> do
                  log $ "wsApp:: " <> show err
                  void $ atomically $ swapTVar currentWsConn Nothing
        loop
    -- WS.sendTextData conn ("reload" :: Text)
    -- WS.sendClose conn ("close" :: Text)
    httpApp req f = do
      print $ Wai.pathInfo req
      let mr = fromSlug $ fromString . toString <$> Wai.pathInfo req
      (status, v) <- case mr of
        Nothing ->
          pure (H.status404, "No route")
        Just r -> do
          (H.status200,) <$> routeBS r
      f $ Wai.responseLBS status [(H.hContentType, "text/html")] v
    routeBS :: route -> IO LByteString
    routeBS r = do
      s <- readTVarIO $ changingTVar $ emaModel ema
      pure $ emaRenderRoute ema s r <> wsClientShim
    -- TODO: Auto reconnect when server reloads
    wsClientShim =
      encodeUtf8
        [text|
        <script>
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

        function refreshPage() {
          // The setTimeout is necessary, otherwise reload will hang forever (at
          // least on Brave browser)
          // 
          // The delayedRefresh trick (5000 and 2000) is for cases when the
          // server hasn't reloaded fast enough, but the browser hangs forever
          // in reload refresh state.
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

        window.onpageshow = () => {
          console.log("ema: Opening ws conn");
          var ws = new WebSocket("ws://" + window.location.host);
          ws.onopen = () => {
            console.log("ema: Observing server for changes");
            ws.send(document.location.pathname);
          };
          ws.onclose = () => {
            console.log("ema: closed; reloading..");
            refreshPage();
          };
          ws.onmessage = evt => {
            // console.log(evt.data);
            console.log("ema: Resetting HTML body")
            setInnerHtml(document.documentElement, evt.data);
            // document.documentElement.innerHTML = evt.data;
            // ws.close();
            // history.go(0);
          };
          window.onbeforeunload = evt => { ws.close(); };
          window.onpagehide = evt => { ws.close(); };
        };
        </script>
        |]
