module Ema.Route.Prism (
  module X,

  -- * Handy encoders
  eitherRoutePrism,

  -- * Handy lenses
  htmlSuffixPrism,
  stringIso,
  showReadPrism,
) where

import Data.Text qualified as T
import Ema.Route.Prism.Check as X
import Ema.Route.Prism.Type as X
import Optics.Core (Iso', Prism', iso, preview, prism', review)

stringIso :: (ToString a, IsString a) => Iso' String a
stringIso = iso fromString toString

showReadPrism :: (Show a, Read a) => Prism' String a
showReadPrism = prism' show readMaybe

htmlSuffixPrism :: Prism' FilePath FilePath
htmlSuffixPrism = prism' (<> ".html") (fmap toString . T.stripSuffix ".html" . toText)

{- | Returns a new route `Prism_` that supports *either* of the input routes.

  The resulting route `Prism_`'s model type becomes the *product* of the input models.
-}
eitherRoutePrism ::
  (a -> Prism_ FilePath r1) ->
  (b -> Prism_ FilePath r2) ->
  ((a, b) -> Prism_ FilePath (Either r1 r2))
eitherRoutePrism enc1 enc2 (m1, m2) =
  toPrism_ $ eitherPrism (fromPrism_ $ enc1 m1) (fromPrism_ $ enc2 m2)

eitherPrism :: Prism' FilePath a -> Prism' FilePath b -> Prism' FilePath (Either a b)
eitherPrism p1 p2 =
  prism'
    ( either
        (review p1)
        (review p2)
    )
    ( \fp ->
        asum
          [ Left <$> preview p1 fp
          , Right <$> preview p2 fp
          ]
    )
