{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

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
  )
where

import Control.Lens (iso)
import Control.Monad.Logger
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Some (Some)
import Data.Text qualified as T
import Ema.Asset (Asset (AssetGenerated), Format (Html))
import Ema.CLI qualified as CLI
import Ema.Route
  ( Mergeable (merge),
    RouteEncoder,
    leftRouteEncoder,
    mapRouteEncoder,
    rightRouteEncoder,
    singletonRouteEncoder,
  )
import System.FilePath ((</>))
import Text.Show (Show (show))
import UnliftIO (MonadUnliftIO, race, race_)
import UnliftIO.Concurrent (threadDelay)

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
        SiteM a r m (NonEmptyLVar m a)
      )

type NonEmptyLVar m a =
  ( -- Initial value
    a,
    -- Generator for subsequent values
    LVar a -> m ()
  )

runModelManager :: (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) => ModelManager a r -> Some CLI.Action -> RouteEncoder a r -> m (NonEmptyLVar m a)
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
  pure (x, f)

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
    (v1, k1) <- runModelManager r1 cliAct $ leftRouteEncoder enc
    (v2, k2) <- runModelManager r2 cliAct $ rightRouteEncoder enc
    l1 <- LVar.empty
    l2 <- LVar.empty
    LVar.set l1 v1
    LVar.set l2 v2
    let v = (v1, v2)
        k lvar = runSiteM cliAct enc $ do
          let keepAlive src = do
                -- TODO: DRY with App.hs
                logWarnNS src "modelPatcher exited; no more model updates."
                threadDelay maxBound
          race_
            ( race_
                (k1 l1 >> keepAlive "siteLeftTODO")
                (k2 l2 >> keepAlive "siteRightTODO")
            )
            ( do
                sub1 <- LVar.addListener l1
                sub2 <- LVar.addListener l2
                forever $
                  race
                    (LVar.listenNext l1 sub1)
                    (LVar.listenNext l2 sub2)
                    >>= \case
                      Left a -> LVar.modify lvar $ first (const a)
                      Right b -> LVar.modify lvar $ second (const b)
            )
    pure (v, k)

-- | Transform the given site such that all of its routes are encoded to be
-- under the given prefix.
mountUnder :: forall r a. String -> Site a r -> Site a (PrefixedRoute r)
mountUnder prefix Site {..} =
  Site siteName siteRender' siteModelManager' (toRouteEncoder siteRouteEncoder)
  where
    siteModelManager' :: ModelManager a (PrefixedRoute r)
    siteModelManager' = ModelManager $ do
      enc :: RouteEncoder a (PrefixedRoute r) <- askRouteEncoder
      cliAct <- askCLIAction
      lift $ runModelManager siteModelManager cliAct (fromRouteEncoder enc)
    siteRender' = SiteRender $ \model r -> do
      rEnc <- askRouteEncoder
      cliAct <- askCLIAction
      pure $ runSiteRender siteRender cliAct (fromRouteEncoder rEnc) model (_prefixedRouteRoute r)
    toRouteEncoder :: RouteEncoder a r -> RouteEncoder a (PrefixedRoute r)
    toRouteEncoder =
      mapRouteEncoder
        (iso (prefix </>) $ fmap toString . T.stripPrefix (toText $ prefix <> "/") . toText)
        (iso (Just . PrefixedRoute prefix) _prefixedRouteRoute)
        id
    -- This coerces the r, but without losing the encoding.
    fromRouteEncoder :: RouteEncoder a (PrefixedRoute r) -> RouteEncoder a r
    fromRouteEncoder =
      mapRouteEncoder (iso id Just) (iso (Just . _prefixedRouteRoute) $ PrefixedRoute prefix) id

-- | A route that is prefixed at some URL prefix
data PrefixedRoute r = PrefixedRoute
  { _prefixedRoutePrefix :: String,
    _prefixedRouteRoute :: r
  }
  deriving stock (Eq, Ord)

instance (Show r) => Show (PrefixedRoute r) where
  show (PrefixedRoute p r) = p <> ":" <> Text.Show.show r
