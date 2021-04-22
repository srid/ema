{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Route
  ( routeUrl,
    routeFile,
    Slug (unSlug),
    UrlStrategy (..),
  )
where

import Data.Default (def)
import Ema.Class
import Ema.Route.Slug (Slug (unSlug))
import Ema.Route.UrlStrategy
  ( UrlStrategy (..),
    slugFileWithStrategy,
    slugUrlWithStrategy,
  )

routeUrl :: forall a. Ema a => Route a -> Text
routeUrl r =
  slugUrlWithStrategy def (encodeRoute @a r)

routeFile :: forall a. Ema a => Route a -> FilePath
routeFile r =
  slugFileWithStrategy def (encodeRoute @a r)
