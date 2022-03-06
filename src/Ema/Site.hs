{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Ema.Site
  ( Site (..),

    -- * Creating sites
    singlePageSite,
    constModel,
    runModelManager,
    ModelManager (ModelManager),
    SiteRender (SiteRender),
    runSiteRender,
    MonadSite (askRouteEncoder, askCLIAction),

    -- * Combinators
    mountUnder,
    PrefixedRoute (..),
    toPrefixedRouteEncoder,
    fromPrefixedRouteEncoder,
  )
where

import Control.Lens (iso)
import Control.Monad.Logger
import Data.Some (Some)
import Data.Text qualified as T
import Ema.Asset (Asset (AssetGenerated), Format (Html))
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic (Dynamic))
import Ema.Route
  ( Mergeable (merge),
    RouteEncoder,
    leftRouteEncoder,
    mapRouteEncoder,
    rightRouteEncoder,
    singletonRouteEncoder,
  )
import GHC.TypeLits
import System.FilePath ((</>))
import Text.Show (Show (show))
import UnliftIO (MonadUnliftIO)

newtype SiteName = SiteName Text
  deriving newtype (Eq, Show, Ord, IsString, ToString)

instance Semigroup SiteName where
  SiteName s1 <> SiteName s2 = SiteName $ s1 <> "+" <> s2

-- | A self-contained Ema site.
data Site a r = Site
  { siteName :: SiteName,
    siteRender :: SiteRender a r,
    siteModelManager :: ModelManager a r,
    siteRouteEncoder :: RouteEncoder a r
  }

class Monad m => MonadSite m a r | m -> a, m -> r where
  askCLIAction :: m (Some CLI.Action)
  askRouteEncoder :: m (RouteEncoder a r)

-- Just a monad to give access to cli action and route encoder.
newtype SiteM a r m x = SiteM (ReaderT (Some CLI.Action, RouteEncoder a r) m x)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadTrans,
      -- MonadReader (Some CLI.Action, RouteEncoder a r),
      MonadIO,
      MonadLogger,
      MonadLoggerIO,
      MonadUnliftIO
    )

runSiteM :: Some CLI.Action -> RouteEncoder a r -> SiteM a r m x -> m x
runSiteM cliAct enc (SiteM m) =
  runReaderT m (cliAct, enc)

instance Monad m => MonadSite (SiteM a r m) a r where
  askCLIAction = SiteM $ asks fst
  askRouteEncoder = SiteM $ asks snd

newtype ModelManager a r
  = ModelManager
      ( forall m.
        ( MonadIO m,
          MonadUnliftIO m,
          MonadLoggerIO m
        ) =>
        SiteM a r m (Dynamic m a)
      )

runModelManager :: (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) => ModelManager a r -> Some CLI.Action -> RouteEncoder a r -> m (Dynamic m a)
runModelManager (ModelManager f) cliAct enc = runSiteM cliAct enc f

newtype SiteRender a r
  = SiteRender
      ( forall m.
        MonadSite m a r =>
        a ->
        r ->
        m (Asset LByteString)
      )

runSiteRender :: SiteRender a r -> Some CLI.Action -> RouteEncoder a r -> a -> r -> Asset LByteString
runSiteRender (SiteRender f) cliAct enc x r = runIdentity $ runSiteM cliAct enc $ f x r

-- | Create a site with a single 'index.html' route, whose contents is specified
-- by the given function.
singlePageSite :: SiteName -> LByteString -> Site () ()
singlePageSite name render =
  Site
    { siteName = name,
      siteRender =
        SiteRender $ \() () -> pure $ AssetGenerated Html render,
      siteModelManager =
        constModel (),
      siteRouteEncoder =
        singletonRouteEncoder
    }

constModel :: a -> ModelManager a r
constModel x = ModelManager $ do
  let f _ = pure ()
  -- TODO: log it
  pure $ Dynamic (x, f)

instance Mergeable Site where
  merge site1 site2 =
    Site
      ((<>) (siteName site1) (siteName site2))
      (merge (siteRender site1) (siteRender site2))
      (merge (siteModelManager site1) (siteModelManager site2))
      (merge (siteRouteEncoder site1) (siteRouteEncoder site2))

instance Mergeable SiteRender where
  merge r1 r2 = SiteRender $ \x r' -> do
    enc <- askRouteEncoder
    cliAct <- askCLIAction
    pure $ case r' of
      Left r -> runSiteRender r1 cliAct (leftRouteEncoder enc) (fst x) r
      Right r -> runSiteRender r2 cliAct (rightRouteEncoder enc) (snd x) r

instance Mergeable ModelManager where
  merge r1 r2 = ModelManager $ do
    cliAct <- askCLIAction
    enc <- askRouteEncoder
    x1 <- lift $ runModelManager r1 cliAct $ leftRouteEncoder enc
    x2 <- lift $ runModelManager r2 cliAct $ rightRouteEncoder enc
    pure $ (,) <$> x1 <*> x2

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
