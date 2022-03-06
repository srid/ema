{-# LANGUAGE RecordWildCards #-}

module Ema.Mount
  ( mountUnder,
    PrefixedRoute (..),
    toPrefixedRouteEncoder,
    fromPrefixedRouteEncoder,
  )
where

import Control.Lens (iso)
import Data.Text qualified as T
import Ema.Route
  ( RouteEncoder,
    mapRouteEncoder,
  )
import Ema.Site
  ( ModelManager (..),
    MonadSite (askCLIAction, askRouteEncoder),
    Site (..),
    SiteRender (SiteRender),
    runModelManager,
    runSiteRender,
  )
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.FilePath ((</>))
import Text.Show (Show (show))

-- | Transform the given site such that all of its routes are encoded to be
-- under the given prefix.
mountUnder :: forall prefix r a. KnownSymbol prefix => Site a r -> Site a (PrefixedRoute prefix r)
mountUnder Site {..} =
  Site siteName siteRender' siteModelManager' (toPrefixedRouteEncoder siteRouteEncoder)
  where
    siteModelManager' :: ModelManager a (PrefixedRoute prefix r)
    siteModelManager' = ModelManager $ do
      enc :: RouteEncoder a (PrefixedRoute prefix r) <- askRouteEncoder
      cliAct <- askCLIAction
      lift $ runModelManager siteModelManager cliAct (fromPrefixedRouteEncoder enc)
    siteRender' = SiteRender $ \model r -> do
      rEnc <- askRouteEncoder
      cliAct <- askCLIAction
      pure $ runSiteRender siteRender cliAct (fromPrefixedRouteEncoder rEnc) model (unPrefixedRoute r)

toPrefixedRouteEncoder :: forall prefix r a. KnownSymbol prefix => RouteEncoder a r -> RouteEncoder a (PrefixedRoute prefix r)
toPrefixedRouteEncoder =
  let prefix = symbolVal (Proxy @prefix)
   in mapRouteEncoder
        (iso (prefix </>) $ fmap toString . T.stripPrefix (toText $ prefix <> "/") . toText)
        (iso (Just . PrefixedRoute) unPrefixedRoute)
        id

-- This coerces the r, but without losing the encoding.
fromPrefixedRouteEncoder :: forall prefix r a. RouteEncoder a (PrefixedRoute prefix r) -> RouteEncoder a r
fromPrefixedRouteEncoder =
  mapRouteEncoder (iso id Just) (iso (Just . unPrefixedRoute) PrefixedRoute) id

-- | A route that is prefixed at some URL prefix
newtype PrefixedRoute (prefix :: Symbol) r = PrefixedRoute {unPrefixedRoute :: r}
  deriving newtype (Eq, Ord)

instance (Show r, KnownSymbol prefix) => Show (PrefixedRoute prefix r) where
  show (PrefixedRoute r) = symbolVal (Proxy @prefix) <> ":" <> Text.Show.show r
