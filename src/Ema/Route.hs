{-# LANGUAGE QuasiQuotes #-}

module Ema.Route
  ( IsRoute (..),
    routeUrl,
    routeFile,
    Slug (unSlug),
    UrlStrategy (..),
  )
where

import Data.Default (def)
import Ema.Route.Slug (Slug (unSlug))
import Ema.Route.UrlStrategy
  ( UrlStrategy (..),
    slugFileWithStrategy,
    slugUrlWithStrategy,
  )

class IsRoute r where
  -- | Determine the route for the given URL slug
  fromSlug :: [Slug] -> Maybe r

  -- | The URL slug to use for a given route.
  toSlug :: r -> [Slug]

  -- | Linking strategy to use
  urlStrategy :: r -> UrlStrategy
  urlStrategy = def

-- | Relative URL to use in "href"
routeUrl :: IsRoute r => r -> Text
routeUrl r =
  slugUrlWithStrategy (urlStrategy r) (toSlug r)

-- | Relative file path to .html file correspondong to this route.
routeFile :: IsRoute r => r -> FilePath
routeFile r =
  slugFileWithStrategy (urlStrategy r) (toSlug r)

-- The unit route is used for sites that have a single route, i.e. index.html.
instance IsRoute () where
  fromSlug = \case
    [] -> Just ()
    _ -> Nothing
  toSlug () = []
