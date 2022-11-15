module Ema.Route.Lib.Folder (
  FolderRoute (FolderRoute, unFolderRoute),
  prefixRoutePrism,
) where

import Data.Text qualified as T
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism (Prism_, mapRoutePrism)
import Ema.Site (EmaSite (..), EmaStaticSite)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Optics.Core (coercedTo, prism', (%))
import System.FilePath ((</>))
import Text.Show (Show (show))

-- | A route that is prefixed at some URL prefix
newtype FolderRoute (prefix :: Symbol) r = FolderRoute {unFolderRoute :: r}
  deriving newtype (Eq, Ord, Generic)

instance (Show r, KnownSymbol prefix) => Show (FolderRoute prefix r) where
  show (FolderRoute r) = symbolVal (Proxy @prefix) <> "/:" <> Text.Show.show r

instance (IsRoute r, KnownSymbol prefix) => IsRoute (FolderRoute prefix r) where
  type RouteModel (FolderRoute prefix r) = RouteModel r
  routePrism = prefixRoutePrism @prefix @r $ routePrism @r
  routeUniverse m = FolderRoute <$> routeUniverse @r m

instance (EmaStaticSite r, KnownSymbol prefix) => EmaSite (FolderRoute prefix r) where
  type SiteArg (FolderRoute prefix r) = SiteArg r
  siteInput cliAct =
    siteInput @r cliAct
  siteOutput rp m r =
    siteOutput @r (rp % coercedTo) m (unFolderRoute r)

-- | Prefix the encoding of the given route prism.
prefixRoutePrism ::
  forall prefix r.
  KnownSymbol prefix =>
  (RouteModel r -> Prism_ FilePath r) ->
  (RouteModel r -> Prism_ FilePath (FolderRoute prefix r))
prefixRoutePrism =
  mapRoutePrism
    (prism' (prefix </>) stripPrefix)
    coercedTo
    id
  where
    prefix = symbolVal (Proxy @prefix)
    stripPrefix =
      fmap toString . T.stripPrefix (toText $ prefix <> "/") . toText
