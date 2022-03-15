module Ema.Route.Encoder where

import Control.Monad.Writer (Writer, tell)
import Data.Text qualified as T
import Optics.Core
  ( Prism',
    preview,
    prism',
    review,
  )

-- | A `Prism` with a context
-- This can't actually be a prism due to coercion problems. See `toPrism`.
type CtxPrism ctx s a =
  -- Prism' (ctx, s) (ctx, a)
  (ctx -> a -> s, ctx -> s -> Maybe a)

toPrism :: CtxPrism ctx s a -> Prism' (ctx, s) (ctx, a)
toPrism (f, g) = prism' (\(ctx, a) -> (ctx, f ctx a)) (\(ctx, s) -> (ctx,) <$> g ctx s)

partialIsoIsLawfulFor :: forall ctx s a. (Eq a, Eq s, Show a, ToText s) => CtxPrism ctx s a -> ctx -> a -> s -> Writer [Text] Bool
partialIsoIsLawfulFor (toPrism -> p) ctx a s = do
  tell . one $ "Testing Partial ISO law for " <> show a <> " and " <> toText s
  let s' :: s = snd $ review p (ctx, a)
  tell . one $ "Route's actual encoding: " <> toText s'
  let ma' :: Maybe a = snd <$> preview p (ctx, s')
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
pimap iso1 iso2 h (toPrism -> cprism) =
  (enc', dec')
  where
    enc' :: y -> d -> b
    enc' m r =
      let r' :: c = review iso2 r
          m' :: x = h m
          a = snd $ review cprism (m', r')
       in review iso1 a
    dec' :: y -> b -> Maybe d
    dec' m fp = do
      fp' <- preview iso1 fp
      (_, r) <- preview cprism (h m, fp')
      preview iso2 r

newtype RouteEncoder a r = RouteEncoder (CtxPrism a FilePath r)

mapRouteEncoder ::
  Prism' FilePath FilePath ->
  Prism' r1 r2 ->
  (b -> a) ->
  RouteEncoder a r1 ->
  RouteEncoder b r2
mapRouteEncoder fpIso rIso mf (RouteEncoder enc) =
  RouteEncoder $ pimap fpIso rIso mf enc

unsafeMkRouteEncoder :: (ctx -> a -> FilePath) -> (ctx -> FilePath -> Maybe a) -> RouteEncoder ctx a
unsafeMkRouteEncoder x y =
  RouteEncoder (x, y)

encodeRoute :: RouteEncoder model r -> model -> r -> FilePath
encodeRoute (RouteEncoder (toPrism -> enc)) m r = snd $ review (enc) (m, r)

decodeRoute :: RouteEncoder model r -> model -> FilePath -> Maybe r
decodeRoute (RouteEncoder (toPrism -> enc)) m s = snd <$> preview (enc) (m, s)

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
