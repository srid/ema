{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Route
  ( HtmlRoute (..),
    routeUrl,
    routeFile,
    Slug (unSlug),
    decodeSlug,
    encodeSlug,
    UrlStrategy (..),
  )
where

import Data.Default (def)
import Ema.Route.Slug (Slug (unSlug), decodeSlug, encodeSlug)
import Ema.Route.UrlStrategy
  ( UrlStrategy (..),
    slugFileWithStrategy,
    slugRelUrlWithStrategy,
  )

-- | Route to the generated HTML file.
class HtmlRoute route where
  -- How to convert URLs to/from routes
  encodeRoute :: route -> [Slug]
  decodeRoute :: [Slug] -> Maybe route

-- | The unit model is useful when using Ema in pure fashion (see @Ema.runEmaPure@) with a single route (index.html) only.
instance HtmlRoute () where
  encodeRoute () = []
  decodeRoute = \case
    [] -> Just ()
    _ -> Nothing

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
routeUrl :: forall r. HtmlRoute r => r -> Text
routeUrl r =
  slugRelUrlWithStrategy def (encodeRoute r)

routeFile :: forall r. HtmlRoute r => r -> FilePath
routeFile r =
  slugFileWithStrategy def (encodeRoute r)
