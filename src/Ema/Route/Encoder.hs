-- | TODO: Export only what's necessary.
module Ema.Route.Encoder where

import Control.Monad.Writer (Writer, runWriter)
import Data.Text qualified as T
import Ema.Route.Encoder.Check qualified as Check
import Optics.Core (
  A_Prism,
  Is,
  NoIx,
  Optic',
  Prism',
  castOptic,
  equality,
  iso,
  preview,
  prism',
  review,
  (%),
 )
import System.FilePath ((</>))

{- | An encoder cum decoder that knows how to convert routes to and from
 filepaths. The conversion depends on the context `a`.
-}
type RouteEncoder a r = a -> Prism_ FilePath r

{- | Isomorphic to `Prism' s a`, but coercion-friendly.

  DerivingVia prevents us from directly using Prism' here
  https://stackoverflow.com/q/71489589/55246
-}
type Prism_ s a = (a -> s, s -> Maybe a)

fromPrism_ :: Prism_ s a -> Prism' s a
fromPrism_ = uncurry prism'

toPrism_ :: Prism' s a -> Prism_ s a
toPrism_ = review &&& preview

-- | Make a `RouteEncoder` manually.
mkRouteEncoder :: (a -> Prism' FilePath r) -> RouteEncoder a r
mkRouteEncoder f = toPrism_ . f

applyRouteEncoder :: RouteEncoder a r -> a -> Prism' FilePath r
applyRouteEncoder f = fromPrism_ . f

{- | fmap over the filepath, route and model in a `RouteEncoder`

  Typically you want to use one of the specific variants below.
-}
mapRouteEncoder ::
  (pr `Is` A_Prism, pf `Is` A_Prism) =>
  Optic' pf NoIx FilePath FilePath ->
  Optic' pr NoIx r1 r2 ->
  (b -> a) ->
  RouteEncoder a r1 ->
  RouteEncoder b r2
mapRouteEncoder fp r m enc =
  mkRouteEncoder $ cpmap (castOptic fp) (castOptic r) m $ applyRouteEncoder enc
  where
    cpmap ::
      forall a b c d x y.
      Prism' b a ->
      Prism' c d ->
      (y -> x) ->
      (x -> Prism' a c) ->
      (y -> Prism' b d)
    cpmap p q f r' ctx =
      p % r' (f ctx) % q

-- | Like `mkRouteEncoder` but ignores the context
prismRouteEncoder :: forall r a. Prism' FilePath r -> RouteEncoder a r
prismRouteEncoder = mkRouteEncoder . const

-- | A route encoder that uses Show/Read to encode/decode.
showReadRouteEncoder :: (Show r, Read r) => RouteEncoder a r
showReadRouteEncoder =
  htmlSuffixEncoder
    & mapRouteEncoder equality (prism' show readMaybe) id

-- | A route encoder that uses @toString@ and @fromString@ to encode and decode respectively.
stringRouteEncoder :: (IsString r, ToString r) => RouteEncoder a r
stringRouteEncoder =
  htmlSuffixEncoder
    & mapRouteEncoder equality (iso fromString toString) id

-- | An encoder that uses the ".html" suffix
htmlSuffixEncoder :: RouteEncoder a FilePath
htmlSuffixEncoder =
  prismRouteEncoder htmlSuffixPrism

htmlSuffixPrism :: Prism' FilePath FilePath
htmlSuffixPrism = prism' (<> ".html") (fmap toString . T.stripSuffix ".html" . toText)

singletonRouteEncoderFrom :: FilePath -> RouteEncoder a ()
singletonRouteEncoderFrom fp =
  prismRouteEncoder $ prism' (\() -> fp) (\s -> guard (s == fp))

-- | Route encoder for single route encoding to 'index.html'
singletonRouteEncoder :: RouteEncoder a ()
singletonRouteEncoder =
  singletonRouteEncoderFrom "index.html"

{- | Returns a new @RouteEncoder@ that supports *either* of the input routes.

  The resulting @RouteEncoder@'s model type becomes the *product* of the input models.
-}
eitherRouteEncoder :: RouteEncoder a r1 -> RouteEncoder b r2 -> RouteEncoder (a, b) (Either r1 r2)
eitherRouteEncoder enc1 enc2 =
  -- TODO: this can be made safe, using lens composition.
  mkRouteEncoder $ \m ->
    let rp1 = applyRouteEncoder enc1 $ fst m
        rp2 = applyRouteEncoder enc2 $ snd m
     in prism'
          ( either
              (review rp1)
              (review rp2)
          )
          ( \fp ->
              asum
                [ Left <$> preview rp1 fp
                , Right <$> preview rp2 fp
                ]
          )

checkRouteEncoderGivenRoute :: (HasCallStack, Eq r, Show r) => RouteEncoder a r -> a -> r -> Either Text ()
checkRouteEncoderGivenRoute enc a r =
  let s = review (applyRouteEncoder enc a) r
   in checkRouteEncoder enc a r s

checkRouteEncoderGivenFilePath ::
  (HasCallStack, Eq r, Show r) =>
  RouteEncoder a r ->
  a ->
  FilePath ->
  Either (r, [(FilePath, Text)]) (Maybe r)
checkRouteEncoderGivenFilePath enc a s = do
  -- We should treat /foo, /foo.html and /foo/index.html as equivalent.
  let candidates = [s, s <> ".html", s </> "index.html"]
      rp = applyRouteEncoder enc a
  case asum (preview rp <$> candidates) of
    Nothing -> pure Nothing
    Just r -> do
      -- All candidates must be checked, and if even one passes - we let this
      -- route go through.
      let (failed, passed) =
            partitionEithers $
              candidates <&> \candidate ->
                case checkRouteEncoder enc a r candidate of
                  Left err -> Left (candidate, err)
                  Right () -> Right ()
      if null passed
        then Left (r, failed)
        else Right (Just r)

checkRouteEncoder :: (Eq r, Show r) => RouteEncoder a r -> a -> r -> FilePath -> Either Text ()
checkRouteEncoder p a r s =
  let (valid, checkLog) =
        runWriter $ routeEncoderIsLawfulFor p a r s
   in if valid
        then Right ()
        else Left $ "Encoding for route '" <> show r <> "' is not isomorphic:\n - " <> T.intercalate "\n - " checkLog

{- | Check if the @CtxPrism@ is lawful.

  A @CtxPrism@ is lawful if its conversions both the ways form an isomorphism
  for a given value.

  Returns a Writer reporting logs.
-}
routeEncoderIsLawfulFor ::
  forall ctx a.
  (Eq a, Show a) =>
  RouteEncoder ctx a ->
  ctx ->
  a ->
  FilePath ->
  Writer [Text] Bool
routeEncoderIsLawfulFor enc =
  Check.prismIsLawfulFor . applyRouteEncoder enc
