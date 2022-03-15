-- | TODO: Export only what's necessary.
module Ema.Route.Encoder where

import Control.Monad.Writer (Writer)
import Data.Text qualified as T
import Ema.Route.CtxPrism
  ( CtxPrism,
    cpmap,
    cpreview,
    creview,
    ctxPrismIsLawfulFor,
  )
import Optics.Core
  ( Prism',
    preview,
    prism',
    review,
  )

-- | An encoder cum decoder that knows how to convert routes to and from
-- filepaths. The conversion depends on the context `a`.
newtype RouteEncoder a r = RouteEncoder (CtxPrism a FilePath r)

mapRouteEncoder ::
  Prism' FilePath FilePath ->
  Prism' r1 r2 ->
  (b -> a) ->
  RouteEncoder a r1 ->
  RouteEncoder b r2
mapRouteEncoder fp r m (RouteEncoder enc) =
  RouteEncoder $ cpmap fp r m enc

unsafeMkRouteEncoder :: (ctx -> a -> FilePath) -> (ctx -> FilePath -> Maybe a) -> RouteEncoder ctx a
unsafeMkRouteEncoder x y =
  RouteEncoder $ \ctx -> (x ctx, y ctx)

encodeRoute :: RouteEncoder model r -> model -> r -> FilePath
encodeRoute (RouteEncoder enc) = creview enc

decodeRoute :: RouteEncoder model r -> model -> FilePath -> Maybe r
decodeRoute (RouteEncoder enc) = cpreview enc

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

-- | TODO: Can do this using generics, on any `f` (not just Either)
--
-- But we as well need model lens for each inner route. Unless we use heterogenous list?
leftRouteEncoder :: RouteEncoder (a, b) (Either r1 r2) -> RouteEncoder a r1
leftRouteEncoder =
  mapRouteEncoder
    (prism' id Just)
    (prism' Left leftToMaybe)
    (,willNotBeUsed)

rightRouteEncoder :: RouteEncoder (a, b) (Either r1 r2) -> RouteEncoder b r2
rightRouteEncoder =
  mapRouteEncoder
    (prism' id Just)
    (prism' Right rightToMaybe)
    (willNotBeUsed,)

singletonRouteEncoderFrom :: FilePath -> RouteEncoder a ()
singletonRouteEncoderFrom fp =
  unsafeMkRouteEncoder (const enc) (const dec)
  where
    enc () = fp
    dec fp' = guard (fp' == fp)

isoRouteEncoder :: Prism' FilePath r -> RouteEncoder () r
isoRouteEncoder p =
  unsafeMkRouteEncoder
    (const $ review p)
    (const $ preview p)

showReadRouteEncoder :: (Show r, Read r) => RouteEncoder () r
showReadRouteEncoder =
  isoRouteEncoder $ prism' ((<> ".html") . show) (readMaybe <=< fmap toString . T.stripSuffix ".html" . toText)

stringRouteEncoder :: (IsString a, ToString a) => RouteEncoder () a
stringRouteEncoder =
  isoRouteEncoder $ prism' ((<> ".html") . toString) (fmap (fromString . toString) . T.stripSuffix ".html" . toText)

-- | Route encoder for single route encoding to 'index.html'
singletonRouteEncoder :: RouteEncoder a ()
singletonRouteEncoder =
  singletonRouteEncoderFrom "index.html"

checkRouteEncoderForSingleRoute :: (Eq route, Show route) => RouteEncoder model route -> model -> route -> FilePath -> Writer [Text] Bool
checkRouteEncoderForSingleRoute (RouteEncoder piso) = ctxPrismIsLawfulFor piso

-- FIXME: a design hack necessitates it.
willNotBeUsed :: HasCallStack => a
willNotBeUsed = error "This value will not be used"
