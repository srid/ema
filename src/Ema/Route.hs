{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Route
  ( routeUrl,
    routeUrlWith,
    UrlStrategy (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.Types (Parser)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Ema.Class (Ema (encodeRoute))
import Ema.Route.Slug (unicodeNormalize)
import qualified Network.URI.Encode as UE

data UrlStrategy
  = UrlPretty
  | UrlDirect
  deriving (Eq, Show, Ord)

instance FromJSON UrlStrategy where
  parseJSON val =
    f UrlPretty "pretty" val <|> f UrlDirect "direct" val
    where
      f :: UrlStrategy -> Text -> Value -> Parser UrlStrategy
      f c s v = do
        x <- parseJSON v
        guard $ x == s
        pure c

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
--
-- TODO: Allow a way to configure disabling stripping of .html, since not all
-- static site hosts support pretty URLs.
routeUrlWith :: forall r model. Ema model r => UrlStrategy -> model -> r -> Text
routeUrlWith urlStrategy model =
  relUrlFromPath . encodeRoute model
  where
    relUrlFromPath :: FilePath -> Text
    relUrlFromPath fp =
      case T.stripSuffix (urlStrategySuffix urlStrategy) (toText fp) of
        Just htmlFp ->
          case nonEmpty (UE.encodeText . unicodeNormalize <$> T.splitOn "/" htmlFp) of
            Nothing ->
              ""
            Just (removeLastIf "index" -> partsSansIndex) ->
              T.intercalate "/" partsSansIndex
        Nothing ->
          T.intercalate "/" $ UE.encodeText . unicodeNormalize <$> T.splitOn "/" (toText fp)
      where
        removeLastIf :: Eq a => a -> NonEmpty a -> [a]
        removeLastIf x xs =
          if NE.last xs == x
            then NE.init xs
            else toList xs
        urlStrategySuffix = \case
          UrlPretty -> ".html"
          UrlDirect -> ""

routeUrl :: forall r model. Ema model r => model -> r -> Text
routeUrl =
  routeUrlWith UrlPretty