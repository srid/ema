module Ema.Route.Lib.Extra.SlugRoute (
  SlugRoute,
  mkSlugRoute,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism (htmlSuffixPrism, toPrism_)
import Network.URI.Slug (Slug)
import Network.URI.Slug qualified as Slug
import Optics.Core (prism', (%))
import System.FilePath (splitExtension, splitPath)

{- | Route to a file that is associated with a value of type `a`.

 A route to foo/bar/qux.md, for instance, is encoded as /foo/bar/qux. ie., the
 extension is dropped.
-}
newtype SlugRoute (a :: Type) = SlugRoute {unSlugRoute :: NonEmpty Slug}
  deriving stock (Eq, Ord, Show, Generic)

instance IsRoute (SlugRoute a) where
  type RouteModel (SlugRoute a) = Map (SlugRoute a) a
  routePrism m =
    let encode (SlugRoute slugs) =
          toString $ T.intercalate "/" $ Slug.unSlug <$> toList slugs
        decode fp = do
          guard $ not $ null fp
          slugs <- nonEmpty $ fromString . toString <$> T.splitOn "/" (toText fp)
          let r = SlugRoute slugs
          guard $ Map.member r m
          pure r
     in toPrism_ $ htmlSuffixPrism % prism' encode decode
  routeUniverse = Map.keys

mkSlugRoute :: forall a. FilePath -> Maybe (String, SlugRoute a)
mkSlugRoute (splitExtension -> (ext', relFp)) = do
  let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath relFp
  (ext',) <$> viaNonEmpty SlugRoute slugs
