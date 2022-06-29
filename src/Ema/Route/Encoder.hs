module Ema.Route.Encoder (
  module X,

  -- * Handy encoders
  prismRouteEncoder,
  showReadRouteEncoder,
  stringRouteEncoder,
  htmlSuffixEncoder,
  htmlSuffixPrism,
  singletonRouteEncoder,
  singletonRouteEncoderFrom,
  eitherRouteEncoder,
) where

import Data.Text qualified as T
import Ema.Route.Encoder.Check as X
import Ema.Route.Encoder.Type as X
import Optics.Core (Prism', equality, iso, preview, prism', review)

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
