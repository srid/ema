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
import Ema.Class (Ema (ModelFor, encodeRoute))
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
routeUrlWith :: forall r. Ema r => UrlStrategy -> ModelFor r -> r -> Text
routeUrlWith urlStrategy model =
  relUrlFromPath . encodeRoute model
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

routeUrl :: forall r. Ema r => ModelFor r -> r -> Text
routeUrl =
  routeUrlWith UrlPretty
