{-# LANGUAGE InstanceSigs #-}

module Ema.Route
  ( -- * Create URL from route
    routeUrl,
    routeUrlWith,
    UrlStrategy (..),

    -- * Route encoder
    RouteEncoder,
    unsafeMkRouteEncoder,
    encodeRoute,
    decodeRoute,
    allRoutes,
    defaultEnum,
    singletonRouteEncoder,
    HasRouteEncoder (..),
    mapRouteEncoder,
    leftRouteEncoder,
    rightRouteEncoder,
    Mergeable (merge),

    -- * Internal
    mergeRouteEncoder,
    checkRouteEncoderForSingleRoute,
    PartialIsoFunctor (pimap),
  )
where

import Control.Lens (Iso)
import Control.Lens qualified as Lens
import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Network.URI.Slug qualified as Slug

-- | An Iso that is not necessarily surjective; as well as takes an (unchanging)
-- context value.
--
-- Parse `s` into (optional) `a` which can always be converted to a `s`. The `a`
-- can be enumerated finitely. `ctx` is used to all functions.
-- TODO: Is this isomrophic to `Iso (ctx, a) (Maybe a) s (ctx, s)` (plus, `ctx -> [a]`)?
data PartialIsoEnumerableWithCtx ctx s a
  = PartialIsoEnumerableWithCtx (ctx -> a -> s, ctx -> s -> Maybe a, ctx -> [a])

{-
type T ctx a s = CtxIso ctx a (Maybe a) s s

type CtxIso ctx a b c d = Iso (ctx, a) b c (ctx, d)

type T' ctx a = ctx -> [a]
-}

partialIsoIsLawfulFor :: (Eq a, Eq s) => PartialIsoEnumerableWithCtx ctx s a -> ctx -> a -> s -> Bool
partialIsoIsLawfulFor (PartialIsoEnumerableWithCtx (to, from, _)) ctx a s =
  (s == to ctx a)
    && (Just a == from ctx s)

class PartialIsoFunctor (f :: Type -> Type -> Type -> Type) where
  pimap ::
    Iso a (Maybe a) b b ->
    Iso c c (Maybe d) d ->
    (y -> x) ->
    f x a c ->
    f y b d

instance PartialIsoFunctor PartialIsoEnumerableWithCtx where
  pimap ::
    forall a b c d x y.
    Iso a (Maybe a) b b ->
    Iso c c (Maybe d) d ->
    (y -> x) ->
    PartialIsoEnumerableWithCtx x a c ->
    PartialIsoEnumerableWithCtx y b d
  pimap iso1 iso2 h (PartialIsoEnumerableWithCtx (enc, dec, all_)) =
    PartialIsoEnumerableWithCtx (enc', dec', all_')
    where
      enc' :: y -> d -> b
      enc' m r =
        let r' :: c = Lens.withIso iso2 $ \_ f -> f r
            m' :: x = h m
         in Lens.withIso iso1 $ \f _ -> f $ enc m' r'
      dec' :: y -> b -> Maybe d
      dec' m fp = do
        fp' <- Lens.withIso iso1 $ \_ f -> f fp
        r :: c <- dec (h m) fp'
        Lens.withIso iso2 $ \f _ -> f r
      all_' :: y -> [d]
      all_' m =
        mapMaybe (\x -> Lens.withIso iso2 $ \f _ -> f x) (all_ $ h m)

newtype RouteEncoder a r = RouteEncoder (PartialIsoEnumerableWithCtx a FilePath r)

mapRouteEncoder ::
  Iso FilePath (Maybe FilePath) FilePath FilePath ->
  Iso r1 r1 (Maybe r2) r2 ->
  (b -> a) ->
  RouteEncoder a r1 ->
  RouteEncoder b r2
mapRouteEncoder fpIso rIso mf (RouteEncoder enc) =
  RouteEncoder $ pimap fpIso rIso mf enc

unsafeMkRouteEncoder :: (ctx -> a -> FilePath) -> (ctx -> FilePath -> Maybe a) -> (ctx -> [a]) -> RouteEncoder ctx a
unsafeMkRouteEncoder x y z = RouteEncoder $ PartialIsoEnumerableWithCtx (x, y, z)

class HasRouteEncoder x a r where
  getRouteEncoder :: x -> RouteEncoder a r

instance HasRouteEncoder (RouteEncoder a r) a r where
  getRouteEncoder = id

encodeRoute :: RouteEncoder model r -> model -> r -> FilePath
encodeRoute (RouteEncoder (PartialIsoEnumerableWithCtx (f, _, _))) = f

decodeRoute :: RouteEncoder model r -> model -> FilePath -> Maybe r
decodeRoute (RouteEncoder (PartialIsoEnumerableWithCtx (_, f, _))) = f

allRoutes :: RouteEncoder model r -> model -> [r]
allRoutes (RouteEncoder (PartialIsoEnumerableWithCtx (_, _, f))) = f

-- | Route encoder for single route encoding to 'index.html'
singletonRouteEncoder :: RouteEncoder () ()
singletonRouteEncoder =
  unsafeMkRouteEncoder
    (\() () -> "index.html")
    (\() fp -> guard (fp == "index.html"))
    (\() -> [()])

-- | Class of product-cum-sum indexed types that can be merged.
class Mergeable (f :: Type -> Type -> Type) where
  -- | Merge by treating the first index as product, and second as sum.
  merge :: f a b -> f c d -> f (a, c) (Either b d)

instance Mergeable RouteEncoder where merge = mergeRouteEncoder

-- | Returns a new route encoder that supports either of the input routes.
mergeRouteEncoder :: RouteEncoder a r1 -> RouteEncoder b r2 -> RouteEncoder (a, b) (Either r1 r2)
mergeRouteEncoder enc1 enc2 =
  unsafeMkRouteEncoder
    ( \m ->
        either
          (encodeRoute enc1 (fst m))
          (encodeRoute enc2 (snd m))
    )
    ( \m fp ->
        asum
          [ Left <$> decodeRoute enc1 (fst m) fp,
            Right <$> decodeRoute enc2 (snd m) fp
          ]
    )
    ( \m ->
        mconcat
          [ Left <$> allRoutes enc1 (fst m),
            Right <$> allRoutes enc2 (snd m)
          ]
    )

leftRouteEncoder :: RouteEncoder (a, b) (Either r1 r2) -> RouteEncoder a r1
leftRouteEncoder =
  mapRouteEncoder
    (Lens.iso id Just)
    (Lens.iso leftToMaybe Left)
    (,undefined)

rightRouteEncoder :: RouteEncoder (a, b) (Either r1 r2) -> RouteEncoder b r2
rightRouteEncoder =
  mapRouteEncoder
    (Lens.iso id Just)
    (Lens.iso rightToMaybe Right)
    (undefined,)

-- TODO: Determine this generically somehow
-- See https://github.com/srid/ema/issues/76
defaultEnum :: (Bounded r, Enum r) => [r]
defaultEnum = [minBound .. maxBound]

checkRouteEncoderForSingleRoute :: Eq route => RouteEncoder model route -> model -> route -> FilePath -> Bool
checkRouteEncoderForSingleRoute (RouteEncoder piso) = partialIsoIsLawfulFor piso

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
