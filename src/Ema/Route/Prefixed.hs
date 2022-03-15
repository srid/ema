module Ema.Route.Prefixed
  ( PrefixedRoute (PrefixedRoute, unPrefixedRoute),
    toPrefixedRouteEncoder,
    fromPrefixedRouteEncoder,
  )
where

import Data.Text qualified as T
import Ema.Asset (CanGenerate (generatableRoutes), CanRender (..))
import Ema.Model
  ( HasModel (ModelInput, modelDynamic),
  )
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder
  ( RouteEncoder,
    mapRouteEncoder,
  )
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Optics.Core (prism')
import System.FilePath ((</>))
import Text.Show (Show (show))

instance (HasModel r, KnownSymbol prefix) => HasModel (PrefixedRoute prefix r) where
  type ModelInput (PrefixedRoute prefix r) = ModelInput r
  modelDynamic cliAct enc input =
    modelDynamic @r cliAct (fromPrefixedRouteEncoder enc) input

instance (CanRender r, KnownSymbol prefix) => CanRender (PrefixedRoute prefix r) where
  routeAsset enc m r =
    routeAsset @r (fromPrefixedRouteEncoder enc) m (unPrefixedRoute r)

instance (CanGenerate r, KnownSymbol prefix) => CanGenerate (PrefixedRoute prefix r) where
  generatableRoutes m = PrefixedRoute <$> generatableRoutes @r m

toPrefixedRouteEncoder :: forall prefix r a. KnownSymbol prefix => RouteEncoder a r -> RouteEncoder a (PrefixedRoute prefix r)
toPrefixedRouteEncoder =
  let prefix = symbolVal (Proxy @prefix)
   in mapRouteEncoder
        (prism' (prefix </>) $ fmap toString . T.stripPrefix (toText $ prefix <> "/") . toText)
        (prism' unPrefixedRoute (Just . PrefixedRoute))
        id

-- This coerces the r, but without losing the prefix encoding.
fromPrefixedRouteEncoder :: forall prefix r a. RouteEncoder a (PrefixedRoute prefix r) -> RouteEncoder a r
fromPrefixedRouteEncoder =
  mapRouteEncoder (prism' id Just) (prism' PrefixedRoute (Just . unPrefixedRoute)) id

-- | A route that is prefixed at some URL prefix
newtype PrefixedRoute (prefix :: Symbol) r = PrefixedRoute {unPrefixedRoute :: r}
  deriving newtype (Eq, Ord)

instance (Show r, KnownSymbol prefix) => Show (PrefixedRoute prefix r) where
  show (PrefixedRoute r) = symbolVal (Proxy @prefix) <> "/:" <> Text.Show.show r

instance (IsRoute r, KnownSymbol prefix) => IsRoute (PrefixedRoute prefix r) where
  type RouteModel (PrefixedRoute prefix r) = RouteModel r
  mkRouteEncoder = toPrefixedRouteEncoder @prefix @r @(RouteModel r) $ mkRouteEncoder @r
