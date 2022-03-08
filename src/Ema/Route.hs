{-# LANGUAGE InstanceSigs #-}

module Ema.Route
  ( -- * Create URL from route
    routeUrl,
    routeUrlWith,
    UrlStrategy (..),
    urlToFilePath,
  )
where

import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Ema.Route.Encoder
import Network.URI.Slug qualified as Slug

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
routeUrlWith :: UrlStrategy -> RouteEncoder a r -> a -> r -> Text
routeUrlWith urlStrategy enc model =
  relUrlFromPath . encodeRoute enc model
  where
    relUrlFromPath :: FilePath -> Text
    relUrlFromPath fp =
      case toString <$> T.stripSuffix (urlStrategySuffix urlStrategy) (toText fp) of
        Just htmlFp ->
          case nonEmpty (filepathToUrl htmlFp) of
            Nothing ->
              ""
            Just (removeLastIf "index" -> partsSansIndex) ->
              T.intercalate "/" partsSansIndex
        Nothing ->
          T.intercalate "/" $ filepathToUrl fp
      where
        removeLastIf :: Eq a => a -> NonEmpty a -> [a]
        removeLastIf x xs =
          if NE.last xs == x
            then NE.init xs
            else toList xs
        urlStrategySuffix = \case
          UrlPretty -> ".html"
          UrlDirect -> ""

filepathToUrl :: FilePath -> [Text]
filepathToUrl =
  fmap (Slug.encodeSlug . fromString @Slug.Slug . toString) . T.splitOn "/" . toText

urlToFilePath :: Text -> FilePath
urlToFilePath =
  toString . T.intercalate "/" . fmap (Slug.unSlug . Slug.decodeSlug) . T.splitOn "/"

routeUrl :: RouteEncoder a r -> a -> r -> Text
routeUrl =
  routeUrlWith UrlPretty

-- | How to produce URL paths from routes
data UrlStrategy
  = -- | Use pretty URLs. "foo/bar.html" produces "/foo/bar" as URL.
    UrlPretty
  | -- | Use filepaths as URLs. "foo/bar.html" produces "/foo/bar.html" as URL.
    UrlDirect
  deriving stock (Eq, Show, Ord)

instance FromJSON UrlStrategy where
  parseJSON val =
    f UrlPretty "pretty" val <|> f UrlDirect "direct" val
    where
      f :: UrlStrategy -> Text -> Value -> Parser UrlStrategy
      f c s v = do
        x <- parseJSON v
        guard $ x == s
        pure c
