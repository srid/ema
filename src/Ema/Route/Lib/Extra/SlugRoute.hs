module Ema.Route.Lib.Extra.SlugRoute (
  SlugRoute,
  IsSlugRoute (mkSlugRoute),
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism (htmlSuffixPrism, toPrism_)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.URI.Slug (Slug)
import Network.URI.Slug qualified as Slug
import Optics.Core (prism', (%))
import System.FilePath (splitExtension, splitPath)

{- | Route to a file that has one of disjoint `exts` as extension, and is associated with
 a value of type `a`.

 A route to foo/bar/qux.md, for instance, is encoded as /foo/bar/qux.
-}
newtype SlugRoute (exts :: [Symbol]) (a :: Type) = SlugRoute {unSlugRoute :: NonEmpty Slug}
  deriving stock (Eq, Ord, Show, Generic)

instance IsRoute (SlugRoute exts a) where
  type RouteModel (SlugRoute exts a) = Map (SlugRoute exts a) a
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

class IsSlugRoute a (exts :: [Symbol]) where
  mkSlugRoute :: FilePath -> Maybe (String, SlugRoute exts a)

instance IsSlugRoute a '[] where
  mkSlugRoute _ = Nothing

instance (IsSlugRoute a exts, KnownSymbol ext) => IsSlugRoute a (ext ': exts) where
  mkSlugRoute fp =
    -- Build a SlugRoute assuming `ext` as the file extension; if that fails,
    -- inductively try the rest.
    mkSlugRouteWith (symbolVal (Proxy @ext)) fp <|> (fmap coerce $ mkSlugRoute @a @exts fp)
    where
      mkSlugRouteWith ext (splitExtension -> (ext', relFp)) = do
        guard $ ext' == ext
        let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath relFp
        (ext,) <$> viaNonEmpty SlugRoute slugs
