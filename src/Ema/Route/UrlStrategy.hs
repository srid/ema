{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Ema.Route.UrlStrategy where

import Data.Default (Default, def)
import qualified Data.Text as T
import Ema.Route.Slug (Slug (unSlug))
import System.FilePath (joinPath)

data UrlStrategy
  = -- | URLs always end with a slash, and correspond to index.html in that folder
    UrlStrategy_FolderOnly
  | -- | Pretty URLs without ugly .html ext or slash-suffix
    UrlStrategy_HtmlOnlySansExt
  deriving (Eq, Show, Ord)

instance Default UrlStrategy where
  def = UrlStrategy_HtmlOnlySansExt

routeUrlWithStrategy :: UrlStrategy -> [Slug] -> Text
routeUrlWithStrategy strat slugs =
  case strat of
    UrlStrategy_FolderOnly ->
      "/" <> T.replace "index.html" "" (toText $ routeFileWithStrategy strat slugs)
    UrlStrategy_HtmlOnlySansExt ->
      -- FIXME: This should replace only at the end, not middle
      let fp = toText (routeFileWithStrategy strat slugs)
       in if
              | "index.html" == fp ->
                "/"
              | "/index.html" `T.isSuffixOf` fp ->
                "/" <> T.take (T.length fp - T.length "/index.html") fp
              | ".html" `T.isSuffixOf` fp ->
                "/" <> T.take (T.length fp - T.length ".html") fp
              | otherwise ->
                "/" <> fp

routeFileWithStrategy :: UrlStrategy -> [Slug] -> FilePath
routeFileWithStrategy strat slugs =
  case strat of
    UrlStrategy_FolderOnly ->
      joinPath $ fmap (toString . unSlug) slugs <> ["index.html"]
    UrlStrategy_HtmlOnlySansExt ->
      let (term :| (reverse -> parts)) = fromMaybe ("index" :| []) $ nonEmpty (reverse $ fmap unSlug slugs)
       in joinPath $ fmap toString parts <> [toString term <> ".html"]
