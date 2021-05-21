{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Route
  ( FileRoute (..),
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

-- | Route to a generated file.
class FileRoute route where
  -- | Slug path as well as the extension of the file corresponding to this
  -- route.
  encodeRoute :: route -> ([Slug], String)

  -- | Decode a slug path into a route. The final component of the slug path
  -- will contain the extension if there is any.
  decodeRoute :: [Slug] -> Maybe route

-- | The unit model is useful when using Ema in pure fashion (see @Ema.runEmaPure@) with a single route (index.html) only.
instance FileRoute () where
  encodeRoute () = ([], ".html")
  decodeRoute = \case
    [] -> Just ()
    _ -> Nothing

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
routeUrl :: forall r. FileRoute r => r -> Text
routeUrl r =
  slugRelUrlWithStrategy def (encodeRoute r)

routeFile :: forall r. FileRoute r => r -> FilePath
routeFile r =
  slugFileWithStrategy def (encodeRoute r)
