module Ema.Route.Prefixed (
  PrefixedRoute (PrefixedRoute, unPrefixedRoute),
  prefixRouteEncoder,
) where

import Data.Text qualified as T
import Ema.Asset (CanGenerate (generatableRoutes), CanRender (..))
import Ema.Model (
  HasModel (ModelInput, modelDynamic),
 )
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder (
  RouteEncoder,
  chainRouteEncoder,
  mapRouteEncoder,
 )
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Optics.Core (coercedTo, prism')
import System.FilePath ((</>))
import Text.Show (Show (show))

instance (HasModel r, KnownSymbol prefix) => HasModel (PrefixedRoute prefix r) where
  type ModelInput (PrefixedRoute prefix r) = ModelInput r
  modelDynamic cliAct enc input =
    modelDynamic @r cliAct (chainRouteEncoder coercedTo enc) input

instance (CanRender r, KnownSymbol prefix) => CanRender (PrefixedRoute prefix r) where
  routeAsset enc m r =
    routeAsset @r (chainRouteEncoder coercedTo enc) m (unPrefixedRoute r)

instance (CanGenerate r, KnownSymbol prefix) => CanGenerate (PrefixedRoute prefix r) where
  generatableRoutes m = PrefixedRoute <$> generatableRoutes @r m

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
