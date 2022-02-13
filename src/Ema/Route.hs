{-# LANGUAGE InstanceSigs #-}

module Ema.Route
  ( -- * Create URL from route
    routeUrl,
    routeUrlWith,
    UrlStrategy (..),

    -- * Route encoder
    RouteEncoder (RouteEncoder),
    encodeRoute,
    decodeRoute,
    allRoutes,
    defaultEnum,

    -- * Internal
    mergeRouteEncoder,
    checkRouteEncoderForSingleRoute,
  )
where

import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.Types (Parser)
import Data.Functor.Contravariant
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Network.URI.Slug qualified as Slug

-- | An Iso that is not necessarily surjective; as well as takes an (unchanging)
-- context value.
type PartialIsoEnumerableWithCtx s ctx a = (ctx -> a -> s, ctx -> s -> Maybe a, ctx -> [a])

partialIsoIsLawfulFor :: (Eq a, Eq s) => PartialIsoEnumerableWithCtx s ctx a -> ctx -> a -> s -> Bool
partialIsoIsLawfulFor (to, from, _) ctx a s =
  (s == to ctx a)
    && (Just a == from ctx s)

data RouteEncoder r a = RouteEncoder {unRouteEncoder :: PartialIsoEnumerableWithCtx FilePath a r}

instance Contravariant (RouteEncoder r) where
  contramap :: forall a b. (a -> b) -> RouteEncoder r b -> RouteEncoder r a
  contramap f enc =
    RouteEncoder
      ( \m r ->
          encodeRoute enc (f m) r,
        \m fp ->
          decodeRoute enc (f m) fp,
        \m ->
          allRoutes enc (f m)
      )

encodeRoute :: RouteEncoder r model -> model -> r -> FilePath
encodeRoute (RouteEncoder (f, _, _)) = f

decodeRoute :: RouteEncoder r model -> model -> FilePath -> Maybe r
decodeRoute (RouteEncoder (_, f, _)) = f

allRoutes :: RouteEncoder r model -> model -> [r]
allRoutes (RouteEncoder (_, _, f)) = f

-- | Returns a new route encoder that supports either of the input routes.
mergeRouteEncoder :: RouteEncoder r1 a -> RouteEncoder r2 b -> RouteEncoder (Either r1 r2) (a, b)
mergeRouteEncoder enc1 enc2 =
  RouteEncoder
    ( \m ->
        either
          (encodeRoute enc1 (fst m))
          (encodeRoute enc2 (snd m)),
      \m fp ->
        asum
          [ Left <$> decodeRoute enc1 (fst m) fp,
            Right <$> decodeRoute enc2 (snd m) fp
          ],
      \m ->
        mconcat
          [ Left <$> allRoutes enc1 (fst m),
            Right <$> allRoutes enc2 (snd m)
          ]
    )

-- TODO: Determine this generically somehow
-- See https://github.com/srid/ema/issues/76
defaultEnum :: (Bounded r, Enum r) => [r]
defaultEnum = [minBound .. maxBound]

checkRouteEncoderForSingleRoute :: Eq route => RouteEncoder route model -> model -> route -> FilePath -> Bool
checkRouteEncoderForSingleRoute = partialIsoIsLawfulFor . unRouteEncoder

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
routeUrlWith :: UrlStrategy -> RouteEncoder r a -> a -> r -> Text
routeUrlWith urlStrategy enc model =
  relUrlFromPath . encodeRoute enc model
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

routeUrl :: RouteEncoder r a -> a -> r -> Text
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
