{-# LANGUAGE AllowAmbiguousTypes #-}

module Ema.Server.HTTP where

import Control.Monad.Logger
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Text qualified as T
import Ema.Asset (
  Asset (AssetGenerated, AssetStatic),
  Format (Html, Other),
 )
import Ema.Route.Class (IsRoute (RouteModel, routePrism))
import Ema.Route.Prism (
  fromPrism_,
 )
import Ema.Server.Common
import Ema.Site (EmaStaticSite)
import Network.HTTP.Types qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Middleware.Static qualified as Static
import Optics.Core (review)

httpApp ::
  forall r.
  (Eq r, Show r, IsRoute r, EmaStaticSite r) =>
  (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) ->
  LVar (RouteModel r) ->
  -- The shim to include in every HTML response
  LByteString ->
  Wai.Application
httpApp logger model shim req f = flip runLoggingT logger $ do
  val <- LVar.get model
  let pathInfo = Wai.pathInfo req
      path = T.intercalate "/" pathInfo
      mr = decodeUrlRoute @r val path
  logInfoNS "ema.http" $ "GET " <> path <> " as " <> show mr
  case mr of
    Left err -> do
      logErrorNS "App" $ badRouteEncodingMsg err
      let s = emaErrorHtmlResponse (badRouteEncodingMsg err) <> shim
      liftIO $ f $ Wai.responseLBS H.status500 [(H.hContentType, "text/html")] s
    Right Nothing -> do
      let s = emaErrorHtmlResponse decodeRouteNothingMsg <> shim
      liftIO $ f $ Wai.responseLBS H.status404 [(H.hContentType, "text/html")] s
    Right (Just r) -> do
      renderCatchingErrors val r >>= \case
        AssetStatic staticPath -> do
          let mimeType = Static.getMimeType staticPath
          liftIO $ f $ Wai.responseFile H.status200 [(H.hContentType, mimeType)] staticPath Nothing
        AssetGenerated Html html -> do
          let s = html <> toLazy wsClientHtml <> shim
          liftIO $ f $ Wai.responseLBS H.status200 [(H.hContentType, "text/html")] s
        AssetGenerated Other s -> do
          let mimeType = Static.getMimeType $ review (fromPrism_ $ routePrism val) r
          liftIO $ f $ Wai.responseLBS H.status200 [(H.hContentType, mimeType)] s
