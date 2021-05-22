{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Route
  ( FileRoute (..),
    routeUrl,
    decodeUrlRoute,
    Slug (unSlug),
    decodeSlug,
    encodeSlug,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Ema.Route.Slug (Slug (unSlug), decodeSlug, encodeSlug, unicodeNormalize)
import qualified Network.URI.Encode as UE
import System.FilePath ((</>))

-- | Route to a generated file.
class FileRoute route where
  -- | Slug path as well as the extension of the file corresponding to this
  -- route.
  encodeFileRoute :: route -> FilePath

  -- | Decode a slug path into a route. The final component of the slug path
  -- will contain the extension if there is any.
  decodeFileRoute :: FilePath -> Maybe route

-- | The unit model is useful when using Ema in pure fashion (see @Ema.runEmaPure@) with a single route (index.html) only.
instance FileRoute () where
  encodeFileRoute () = "index.html"
  decodeFileRoute = \case
    "index.html" -> Just ()
    _ -> Nothing

-- | Decode a URL path into a route
decodeUrlRoute :: FileRoute route => Text -> Maybe route
decodeUrlRoute (toString -> s) =
  decodeFileRoute s
    <|> decodeFileRoute (s <> ".html")
    <|> decodeFileRoute (s </> "index.html")

-- | Return the relative URL of the given route
--
-- As the returned URL is relative, you will have to either make it absolute (by
-- prepending with `/`) or set the `<base>` URL in your HTML head element.
routeUrl :: forall r. FileRoute r => r -> Text
routeUrl =
  relUrlFromPath . encodeFileRoute
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