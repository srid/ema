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
    slugRelUrlWithStrategy,
  )

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
routeUrl :: forall a r. Ema a r => r -> Text
routeUrl r =
  slugRelUrlWithStrategy def (encodeRoute @a r)

routeFile :: forall a r. Ema a r => r -> FilePath
routeFile r =
  slugFileWithStrategy def (encodeRoute @a r)
