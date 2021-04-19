{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Ema.Route where

import Data.Default (Default, def)
import qualified Data.Text as T
import System.FilePath (joinPath)

-- ---- [Slug] ----

newtype Slug = Slug {unSlug :: Text}
  deriving (Eq)

instance IsString Slug where
  fromString :: HasCallStack => String -> Slug
  fromString (toText -> s) =
    if "/" `T.isInfixOf` s
      then error ("Slug cannot contain a slash: " <> s)
      else Slug s

-- ---- [Route] ----

class IsRoute r where
  -- | Determine the route for the given URL slug
  fromSlug :: [Slug] -> Maybe r

  -- | The URL slug to use for a given route.
  toSlug :: r -> [Slug]

  -- | Relative URL to use in "href"
  routeUrl :: r -> Text
  routeUrl =
    routeUrlWithStrategy def

  -- | Relative file path to .html file correspondong to this route.
  routeFile :: r -> FilePath
  routeFile =
    routeFileWithStrategy def

instance IsRoute () where
  fromSlug = \case
    [] -> Just ()
    _ -> Nothing
  toSlug () = []

-- ---- [UrlStrategy] ----

data UrlStrategy
  = UrlStrategy_FolderOnly
  | -- | Pretty URLs without ugly .html ext or slash-suffix
    UrlStrategy_HtmlOnlySansExt
  deriving (Eq, Show, Ord)

instance Default UrlStrategy where
  def = UrlStrategy_HtmlOnlySansExt

routeUrlWithStrategy :: IsRoute r => UrlStrategy -> r -> Text
routeUrlWithStrategy strat r =
  case strat of
    UrlStrategy_FolderOnly ->
      "/" <> T.replace "index.html" "" (toText $ routeFile r)
    UrlStrategy_HtmlOnlySansExt ->
      -- FIXME: This should replace only at the end, not middle
      let fp = toText (routeFile r)
       in if
              | "index.html" == fp ->
                "/"
              | "/index.html" `T.isSuffixOf` fp ->
                "/" <> T.take (T.length fp - T.length "/index.html") fp
              | ".html" `T.isSuffixOf` fp ->
                "/" <> T.take (T.length fp - T.length ".html") fp
              | otherwise ->
                "/" <> fp

routeFileWithStrategy :: IsRoute r => UrlStrategy -> r -> FilePath
routeFileWithStrategy strat (toSlug -> slugs) =
  case strat of
    UrlStrategy_FolderOnly ->
      joinPath $ fmap (toString . unSlug) slugs <> ["index.html"]
    UrlStrategy_HtmlOnlySansExt ->
      let (term :| (reverse -> parts)) = fromMaybe ("index" :| []) $ nonEmpty (reverse $ fmap unSlug slugs)
       in joinPath $ fmap toString parts <> [toString term <> ".html"]
