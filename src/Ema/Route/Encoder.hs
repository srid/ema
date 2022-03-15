module Ema.Route.Encoder where

import Control.Monad.Writer (Writer, tell)
import Data.Text qualified as T
import Optics.Core
  ( Prism',
    preview,
    prism',
    review,
    (%),
  )

-- | A `Prism` with a context
-- This can't actually be a prism due to coercion problems. Use `toPrism` & `fromPrism`.
type CtxPrism ctx s a =
  -- Prism' s a
  ctx -> (a -> s, s -> Maybe a)

toPrism :: CtxPrism ctx s a -> ctx -> Prism' s a
toPrism f' ctx = let (f, g) = f' ctx in prism' f g

fromPrism :: (ctx -> Prism' s a) -> CtxPrism ctx s a
fromPrism f' ctx = let p = f' ctx in (review p, preview p)

partialIsoIsLawfulFor :: forall ctx s a. (Eq a, Eq s, Show a, ToText s) => CtxPrism ctx s a -> ctx -> a -> s -> Writer [Text] Bool
partialIsoIsLawfulFor pf ctx a s = do
  let p = toPrism pf ctx
  tell . one $ "Testing Partial ISO law for " <> show a <> " and " <> toText s
  let s' :: s = review p a
  tell . one $ "Route's actual encoding: " <> toText s'
  let ma' :: Maybe a = preview p s'
  tell . one $ "Decoding of that encoding: " <> show ma'
  unless (s == s') $
    tell . one $ "ERR: " <> toText s <> " /= " <> toText s'
  unless (Just a == ma') $
    tell . one $ "ERR: " <> show (Just a) <> " /= " <> show ma'
  pure $ (s == s') && (Just a == ma')

pimap ::
  forall a b c d x y.
  Prism' b a ->
  Prism' c d ->
  (y -> x) ->
  CtxPrism x a c ->
  CtxPrism y b d
pimap p q f r = fromPrism $ \ctx -> p % toPrism r (f ctx) % q

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
  RouteEncoder $ pimap fp r m enc

unsafeMkRouteEncoder :: (ctx -> a -> FilePath) -> (ctx -> FilePath -> Maybe a) -> RouteEncoder ctx a
unsafeMkRouteEncoder x y =
  RouteEncoder $ \ctx -> (x ctx, y ctx)

encodeRoute :: RouteEncoder model r -> model -> r -> FilePath
encodeRoute (RouteEncoder enc) ctx = review (toPrism enc ctx)

decodeRoute :: RouteEncoder model r -> model -> FilePath -> Maybe r
decodeRoute (RouteEncoder enc) ctx = preview (toPrism enc ctx)

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
isoRouteEncoder riso =
  unsafeMkRouteEncoder
    (const $ \r -> review riso r)
    (const $ \fp -> preview riso fp)

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
checkRouteEncoderForSingleRoute (RouteEncoder piso) = partialIsoIsLawfulFor piso

-- FIXME: a design hack necessitates it.
willNotBeUsed :: HasCallStack => a
willNotBeUsed = error "This value will not be used"
