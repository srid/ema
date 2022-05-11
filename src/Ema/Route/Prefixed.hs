module Ema.Route.Prefixed (
  PrefixedRoute (PrefixedRoute, unPrefixedRoute),
  prefixRouteEncoder,
) where

import Data.Text qualified as T
import Ema.Model
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder (
  RouteEncoder,
  mapRouteEncoder,
  mapRouteEncoderRoute,
 )
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Optics.Core (coercedTo, prism')
import System.FilePath ((</>))
import Text.Show (Show (show))

instance (EmaSite r, KnownSymbol prefix) => EmaSite (PrefixedRoute prefix r) where
  type SiteArg (PrefixedRoute prefix r) = SiteArg r
  siteInput cliAct enc =
    siteInput @r cliAct (mapRouteEncoderRoute coercedTo enc)
  siteOutput enc m r =
    siteOutput @r (mapRouteEncoderRoute coercedTo enc) m (unPrefixedRoute r)

-- | Prefix the encoding of the given RouteEncoder.
prefixRouteEncoder :: forall prefix r a. KnownSymbol prefix => RouteEncoder a r -> RouteEncoder a (PrefixedRoute prefix r)
prefixRouteEncoder =
  mapRouteEncoder
    (prism' (prefix </>) stripPrefix)
    coercedTo
    id
  where
    prefix = symbolVal (Proxy @prefix)
    stripPrefix =
      fmap toString . T.stripPrefix (toText $ prefix <> "/") . toText

-- | A route that is prefixed at some URL prefix
newtype PrefixedRoute (prefix :: Symbol) r = PrefixedRoute {unPrefixedRoute :: r}
  deriving newtype (Eq, Ord)

instance (Show r, KnownSymbol prefix) => Show (PrefixedRoute prefix r) where
  show (PrefixedRoute r) = symbolVal (Proxy @prefix) <> "/:" <> Text.Show.show r

instance (IsRoute r, KnownSymbol prefix) => IsRoute (PrefixedRoute prefix r) where
  type RouteModel (PrefixedRoute prefix r) = RouteModel r
  routeEncoder = prefixRouteEncoder @prefix @r @(RouteModel r) $ routeEncoder @r
  allRoutes m = PrefixedRoute <$> allRoutes @r m
