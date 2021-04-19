{-# LANGUAGE QuasiQuotes #-}

module Ema.Route
  ( IsRoute (..),
    Slug (unSlug),
  )
where

import Data.Default (def)
import Ema.Route.Slug (Slug (unSlug))
import Ema.Route.UrlStrategy
  ( routeFileWithStrategy,
    routeUrlWithStrategy,
  )

class IsRoute r where
  -- | Determine the route for the given URL slug
  fromSlug :: [Slug] -> Maybe r

  -- | The URL slug to use for a given route.
  toSlug :: r -> [Slug]

  -- | Relative URL to use in "href"
  routeUrl :: r -> Text
  routeUrl =
    routeUrlWithStrategy def . toSlug

  -- | Relative file path to .html file correspondong to this route.
  routeFile :: r -> FilePath
  routeFile =
    routeFileWithStrategy def . toSlug

-- The unit route is used for sites that have a single route, i.e. index.html.
instance IsRoute () where
  fromSlug = \case
    [] -> Just ()
    _ -> Nothing
  toSlug () = []
