module Ema.Route.Encoder (
  module X,

  -- * Handy encoders
  prismRouteEncoder,
  singletonRouteEncoder,
  eitherRouteEncoder,

  -- * Handy lenses
  htmlSuffixPrism,
  stringIso,
  showReadPrism,
  singletonPrism,
) where

import Data.Text qualified as T
import Ema.Route.Encoder.Check as X
import Ema.Route.Encoder.Type as X
import Optics.Core (Iso', Prism', iso, preview, prism', review)

{- | Like `mkRouteEncoder` but ignores the model context.

  A handy list of `Prism'`'s are provided below. They are typically used in
  conjunction with `htmlSuffixPrism` to creat a encoding for .html routes. For
  example,

  >>> prismRouteEncoder $ htmlSuffixPrism % showReadPrism
-}
prismRouteEncoder :: forall r a. Prism' FilePath r -> RouteEncoder a r
prismRouteEncoder = mkRouteEncoder . const

stringIso :: (ToString a, IsString a) => Iso' String a
stringIso = iso fromString toString

showReadPrism :: (Show a, Read a) => Prism' String a
showReadPrism = prism' show readMaybe

htmlSuffixPrism :: Prism' FilePath FilePath
htmlSuffixPrism = prism' (<> ".html") (fmap toString . T.stripSuffix ".html" . toText)

singletonRouteEncoder :: FilePath -> RouteEncoder a ()
singletonRouteEncoder fp =
  prismRouteEncoder $ singletonPrism fp

singletonPrism :: Eq a => a -> Prism' a ()
singletonPrism x = prism' (\() -> x) (\s -> guard (s == x))

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
