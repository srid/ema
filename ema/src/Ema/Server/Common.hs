{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ema.Server.Common where

import Control.Monad.Logger
import Data.FileEmbed
import Data.Text qualified as T
import Ema.Asset (
  Asset (AssetGenerated),
  Format (Html),
 )
import Ema.Route.Class (IsRoute (RouteModel, routePrism))
import Ema.Route.Prism (
  checkRoutePrismGivenFilePath,
  fromPrism_,
 )
import Ema.Route.Url (urlToFilePath)
import Ema.Site (EmaSite (siteOutput), EmaStaticSite)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catch)

renderCatchingErrors ::
  forall r m.
  ( MonadLoggerIO m
  , MonadUnliftIO m
  , EmaStaticSite r
  ) =>
  RouteModel r ->
  r ->
  m (Asset LByteString)
renderCatchingErrors m r =
  catch (siteOutput (fromPrism_ $ routePrism m) m r) $ \(err :: SomeException) -> do
    -- Log the error first.
    logErrorNS "App" $ show @Text err
    pure
      $ AssetGenerated Html
        . mkHtmlErrorMsg
      $ show @Text err

-- Decode an URL path into a route
--
-- This function is used only in live server. If the route is not
-- isomoprhic, this returns a Left, with the mismatched encoding.
decodeUrlRoute ::
  forall r.
  (Eq r, Show r, IsRoute r) =>
  RouteModel r ->
  Text ->
  Either (BadRouteEncoding r) (Maybe r)
decodeUrlRoute m (urlToFilePath -> s) = do
  case checkRoutePrismGivenFilePath routePrism m s of
    Left (r, log) -> Left $ BadRouteEncoding s r log
    Right mr -> Right mr

-- | A basic error response for displaying in the browser
emaErrorHtmlResponse :: Text -> LByteString
emaErrorHtmlResponse err =
  mkHtmlErrorMsg err <> toLazy wsClientHtml

mkHtmlErrorMsg :: Text -> LByteString
mkHtmlErrorMsg s =
  encodeUtf8 . T.replace "MESSAGE" s . decodeUtf8 $ emaErrorHtml

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

emaErrorHtml :: ByteString
emaErrorHtml = $(embedFile "www/ema-error.html")

wsClientHtml :: ByteString
wsClientHtml = $(embedFile "www/ema-indicator.html")

wsClientJSShim :: Text
wsClientJSShim = decodeUtf8 $(embedFile "www/ema-shim.js")
