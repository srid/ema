{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Route
  ( routeUrl,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Ema.Class (Ema (encodeRoute))
import Ema.Route.Slug (unicodeNormalize)
import qualified Network.URI.Encode as UE

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
--
-- TODO: Allow a way to configure disabling stripping of .html, since not all
-- static site hosts support pretty URLs.
routeUrl :: forall r model. Ema model r => r -> Text
routeUrl =
  relUrlFromPath . encodeRoute
  where
    relUrlFromPath :: FilePath -> Text
    relUrlFromPath fp =
      case T.stripSuffix ".html" (toText fp) of
        Just htmlFp ->
          case nonEmpty (UE.encodeText . unicodeNormalize <$> T.splitOn "/" htmlFp) of
            Nothing ->
              ""
            Just (removeLastIf "index" -> partsSansIndex) ->
              T.intercalate "/" partsSansIndex
        Nothing ->
          UE.encodeText . unicodeNormalize . toText $ fp
      where
        removeLastIf :: Eq a => a -> NonEmpty a -> [a]
        removeLastIf x xs =
          if NE.last xs == x
            then NE.init xs
            else toList xs
