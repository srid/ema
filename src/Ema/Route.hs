{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Route
  ( routeUrl,
    routeFile,
    Slug (unSlug),
    decodeSlug,
    encodeSlug,
    UrlStrategy (..),
  )
where

import Data.Default (def)
import Ema.Class
import Ema.Route.Slug (Slug (unSlug), decodeSlug, encodeSlug)
import Ema.Route.UrlStrategy
  ( UrlStrategy (..),
    slugFileWithStrategy,
    slugUrlWithStrategy,
  )

routeUrl :: forall a r. Ema a r => r -> Text
routeUrl r =
  slugUrlWithStrategy def (encodeRoute @a r)

routeFile :: forall a r. Ema a r => r -> FilePath
routeFile r =
  slugFileWithStrategy def (encodeRoute @a r)
