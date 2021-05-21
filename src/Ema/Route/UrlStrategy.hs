{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Ema.Route.UrlStrategy where

import Data.Default (Default, def)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Ema.Route.Slug (Slug (unSlug), decodeSlug, encodeSlug)
import System.FilePath (joinPath)

data UrlStrategy
  = -- | URLs always end with a slash, and correspond to index.html in that folder
    UrlStrategy_FolderOnly
  | -- | Pretty URLs without ugly .html ext or slash-suffix
    UrlStrategy_HtmlOnlySansExt
  deriving (Eq, Show, Ord)

instance Default UrlStrategy where
  def = UrlStrategy_HtmlOnlySansExt

slugRelUrlWithStrategy :: UrlStrategy -> ([Slug], String) -> Text
slugRelUrlWithStrategy strat (slugs, ".html") =
  case strat of
    UrlStrategy_FolderOnly ->
      T.intercalate "/" (encodeSlug <$> slugs)
    UrlStrategy_HtmlOnlySansExt ->
      case nonEmpty slugs of
        Nothing ->
          ""
        Just (removeLastIf (decodeSlug "index") -> slugsWithoutIndex) ->
          T.intercalate "/" (encodeSlug <$> slugsWithoutIndex)
  where
    removeLastIf :: Eq a => a -> NonEmpty a -> [a]
    removeLastIf x xs =
      if NE.last xs == x
        then NE.init xs
        else toList xs
slugRelUrlWithStrategy _ (slugs, ext) =
  T.intercalate "/" (encodeSlug <$> slugs) <> toText ext

slugFileWithStrategy :: UrlStrategy -> ([Slug], String) -> FilePath
slugFileWithStrategy strat (slugs, ext) =
  case strat of
    UrlStrategy_FolderOnly ->
      joinPath $ fmap (toString . unSlug) slugs <> ["index" <> ext]
    UrlStrategy_HtmlOnlySansExt ->
      let (term :| (reverse -> parts)) = fromMaybe ("index" :| []) $ nonEmpty (reverse $ fmap (toString . unSlug) slugs)
       in joinPath $ parts <> [term <> ext]
