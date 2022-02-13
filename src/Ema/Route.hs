module Ema.Route
  ( -- * Create URL from route
    routeUrl,
    routeUrlWith,
    UrlStrategy (..),

    -- * Route encoder
    RouteEncoder,
    encodeRoute,
    decodeRoute,
    allRoutes,
    defaultEnum,
  )
where

import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Network.URI.Slug qualified as Slug

-- | An Iso that is not necessarily surjective; as well as takes an (unchanging)
-- context value.
type PartialIsoEnumerableWithCtx ctx s a = (ctx -> a -> s, ctx -> s -> Maybe a, ctx -> [a])

_partialIsoIsLawfulForCtx :: Eq a => PartialIsoEnumerableWithCtx ctx s a -> ctx -> Bool
_partialIsoIsLawfulForCtx (to, from, getas) ctx =
  all (\a -> let s = to ctx a in Just a == from ctx s) (getas ctx)

type RouteEncoder model route = PartialIsoEnumerableWithCtx model FilePath route

encodeRoute :: RouteEncoder model r -> model -> r -> FilePath
encodeRoute (f, _, _) = f

decodeRoute :: RouteEncoder model r -> model -> FilePath -> Maybe r
decodeRoute (_, f, _) = f

allRoutes :: RouteEncoder model r -> model -> [r]
allRoutes (_, _, f) = f

defaultEnum :: (Bounded r, Enum r) => [r]
defaultEnum = [minBound .. maxBound]

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
routeUrlWith :: UrlStrategy -> RouteEncoder a r -> a -> r -> Text
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
