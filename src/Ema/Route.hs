module Ema.Route
  ( routeUrl,
    routeUrlWith,
    UrlStrategy (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Ema.Class (Ema (ModelFor))
import Ema.Site
import Network.URI.Slug qualified as Slug

data UrlStrategy
  = UrlPretty
  | UrlDirect
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

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
routeUrlWith :: UrlStrategy -> PartialIsoEnumerableWithCtx (ModelFor r) FilePath r -> ModelFor r -> r -> Text
routeUrlWith urlStrategy (enc, _, _) model =
  relUrlFromPath . enc model
  where
    relUrlFromPath :: FilePath -> Text
    relUrlFromPath fp =
      case T.stripSuffix (urlStrategySuffix urlStrategy) (toText fp) of
        Just htmlFp ->
          case nonEmpty (urlSlugFromText <$> T.splitOn "/" htmlFp) of
            Nothing ->
              ""
            Just (removeLastIf "index" -> partsSansIndex) ->
              T.intercalate "/" partsSansIndex
        Nothing ->
          T.intercalate "/" $ urlSlugFromText <$> T.splitOn "/" (toText fp)
      where
        urlSlugFromText = Slug.encodeSlug . fromString @Slug.Slug . toString
        removeLastIf :: Eq a => a -> NonEmpty a -> [a]
        removeLastIf x xs =
          if NE.last xs == x
            then NE.init xs
            else toList xs
        urlStrategySuffix = \case
          UrlPretty -> ".html"
          UrlDirect -> ""

routeUrl :: PartialIsoEnumerableWithCtx (ModelFor r) FilePath r -> ModelFor r -> r -> Text
routeUrl =
  routeUrlWith UrlPretty
