module Ema.Route.Lib.Folder (
  FolderRoute (FolderRoute, unFolderRoute),
  prefixRouteEncoder,
) where

import Data.Text qualified as T
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder (
  RouteEncoder,
  mapRouteEncoder,
  mapRouteEncoderRoute,
 )
import Ema.Site (EmaSite (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Optics.Core (coercedTo, prism')
import System.FilePath ((</>))
import Text.Show (Show (show))

-- | A route that is prefixed at some URL prefix
newtype FolderRoute (prefix :: Symbol) r = FolderRoute {unFolderRoute :: r}
  deriving newtype (Eq, Ord)

instance (Show r, KnownSymbol prefix) => Show (FolderRoute prefix r) where
  show (FolderRoute r) = symbolVal (Proxy @prefix) <> "/:" <> Text.Show.show r

instance (IsRoute r, KnownSymbol prefix) => IsRoute (FolderRoute prefix r) where
  type RouteModel (FolderRoute prefix r) = RouteModel r
  routeEncoder = prefixRouteEncoder @prefix @r @(RouteModel r) $ routeEncoder @r
  allRoutes m = FolderRoute <$> allRoutes @r m

instance (EmaSite r, KnownSymbol prefix) => EmaSite (FolderRoute prefix r) where
  type SiteArg (FolderRoute prefix r) = SiteArg r
  siteInput cliAct =
    siteInput @r cliAct
  siteOutput enc m r =
    siteOutput @r (mapRouteEncoderRoute coercedTo enc) m (unFolderRoute r)

-- | Prefix the encoding of the given RouteEncoder.
prefixRouteEncoder :: forall prefix r a. KnownSymbol prefix => RouteEncoder a r -> RouteEncoder a (FolderRoute prefix r)
prefixRouteEncoder =
  mapRouteEncoder
    (prism' (prefix </>) stripPrefix)
    coercedTo
    id
  where
    prefix = symbolVal (Proxy @prefix)
    stripPrefix =
      fmap toString . T.stripPrefix (toText $ prefix <> "/") . toText
