{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A very simple markdown website using Ema.Route.Lib.Extra.PandocRoute.

 Also demostrates how to set up a custom server for following the currently open
 note, using a websocket for editor integration.
-}
module Ema.Example.Ex06_Markdown where

import Control.Monad.Logger (LogLevel (..), MonadLoggerIO (..), defaultLoc, logInfoNS)
import Data.Default (Default (..))
import Data.Generics.Sum.Any
import Data.Map (member)
import Ema
import Ema.CLI qualified as CLI
import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.PandocRoute qualified as Pandoc
import Ema.Server (EmaServerOptions (..))
import Ema.Server.WebSocket.Options (EmaWsHandler (..), wsClientJS)
import Network.WebSockets qualified as WS
import Optics.Core ((%))
import System.Directory (makeAbsolute)
import System.FilePath (isAbsolute, isRelative, makeRelative)
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import UnliftIO.Async (race)
import UnliftIO.STM (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)

data Arg = Arg
  { pandocArg :: Pandoc.Arg
  , editorWsAddress :: String
  , editorWsPort :: Int
  }
  deriving stock (Generic)

instance Default Arg where
  def =
    Arg
      { pandocArg =
          def
            { Pandoc.argBaseDir = "src/Ema/Example/Ex06_Markdown"
            }
      , editorWsAddress = "127.0.0.1"
      , editorWsPort = 9160
      }

data Model = Model
  { pandocModel :: Pandoc.Model
  , wsNextRoute :: TChan Route
  }
  deriving stock (Generic)

newtype Route = Route Pandoc.PandocRoute
  deriving stock (Show, Eq, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute
  ''Route
  [t|
    '[ WithModel Model
     , WithSubRoutes
        '[ Pandoc.PandocRoute
         ]
     ]
    |]

instance EmaSite Route where
  type SiteArg Route = Arg

  siteInput act arg = do
    pandocDyn <- siteInput @Pandoc.PandocRoute act (pandocArg arg)
    editorWsDyn <- wsConnDyn arg
    return $ Model <$> pandocDyn <*> editorWsDyn

  siteOutput rp m (Route r) = do
    (pandoc, write) <- siteOutput (rp % _As @"Route") (pandocModel m) r
    let head' = H.title "Basic site" >> H.base ! A.href "/"
        body :: Text = coerce $ write pandoc
        html = RU.renderHtml do
          H.docType
          H.html ! A.lang "en" $ do
            H.head do
              H.meta ! A.charset "UTF-8"
              H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
              head'
            H.body $ H.preEscapedToHtml body
    return $ AssetGenerated Html html

wsConnDyn :: forall m. (MonadLoggerIO m) => Arg -> m (Dynamic m (TChan Route))
wsConnDyn arg = do
  value <- newBroadcastTChanIO
  let manage :: m ()
      manage = do
        logger <- askLoggerIO
        let log = logger defaultLoc "wsConnDyn" LevelInfo
        liftIO $ WS.runServer (editorWsAddress arg) (editorWsPort arg) \pendingConn -> do
          conn :: WS.Connection <- WS.acceptRequest pendingConn
          log "websocket connected"
          WS.withPingThread conn 30 pass $
            void $
              infinitely do
                msg <- liftIO $ toString @Text <$> WS.receiveData conn
                log $ "got message: " <> show msg
                baseDir <- makeAbsolute (Pandoc.argBaseDir $ pandocArg arg)
                let fp = makeRelative baseDir msg
                case Pandoc.mkPandocRoute fp of
                  Just (_, route)
                    -- We should have received an absolute file path inside the base dir
                    | isAbsolute msg && isRelative fp ->
                        atomically $ writeTChan value (Route route)
                  _ -> pass
  return $ Dynamic (value, const manage)

main :: IO ()
main = runWithFollow def

runWithFollow ::
  SiteArg Route ->
  IO ()
runWithFollow input = do
  cli <- CLI.cliAction
  let cfg = SiteConfig cli followServerOptions
  void $ snd <$> runSiteWith @Route cfg input

followServerOptions :: EmaServerOptions Route
followServerOptions = EmaServerOptions wsClientJS followServerHandler

followServerHandler :: EmaWsHandler Route
followServerHandler = EmaWsHandler handle
  where
    defaultHandler = unEmaWsHandler $ def @(EmaWsHandler ())
    handle conn model = do
      either id id <$> race (defaultHandler conn ()) followHandler
      where
        rp = fromPrism_ $ routePrism model
        log = logInfoNS "followServerHandler"
        followHandler = do
          listenerChan <- atomically $ dupTChan $ wsNextRoute model
          route <- atomically $ readTChan listenerChan
          let Route pRoute = route
              path = routeUrl rp route
          if pRoute `member` Pandoc.modelPandocs (pandocModel model)
            then do
              log $ "switching to " <> show pRoute
              liftIO $ WS.sendTextData conn $ "SWITCH " <> path
            else log $ "invalid route " <> show pRoute
          followHandler
