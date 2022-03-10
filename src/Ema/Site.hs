{-# LANGUAGE UndecidableInstances #-}

module Ema.Site
  ( Site (..),

    -- * Creating sites
    singlePageSite,
    constModel,
    runModelManager,
    ModelManager (ModelManager),

    -- * Remove site?
    RenderAsset (..),
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Some (Some)
import Ema.Asset (Asset)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic (Dynamic))
import Ema.Route.Encoder
  ( Mergeable (merge),
    RouteEncoder,
    leftRouteEncoder,
    rightRouteEncoder,
  )
import Ema.Route.Generic (IsRoute (RouteModel))
import UnliftIO (MonadUnliftIO)

newtype SiteName = SiteName Text
  deriving newtype (Eq, Show, Ord, IsString, ToString)

instance Semigroup SiteName where
  SiteName s1 <> SiteName s2 = SiteName $ s1 <> "+" <> s2

-- | A self-contained Ema site.
data Site a r = Site
  { siteName :: SiteName,
    siteModelManager :: ModelManager a r
  }

-- TODO: Have this take a `i` for input to manager?
newtype ModelManager a r
  = ModelManager
      ( forall m.
        ( MonadIO m,
          MonadUnliftIO m,
          MonadLoggerIO m
        ) =>
        Some CLI.Action ->
        RouteEncoder a r ->
        m (Dynamic m a)
      )

runModelManager :: (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) => ModelManager a r -> Some CLI.Action -> RouteEncoder a r -> m (Dynamic m a)
runModelManager (ModelManager f) = f

-- | Create a site with a single 'index.html' route, whose contents is specified
-- by the given function.
singlePageSite :: SiteName -> LByteString -> Site () ()
singlePageSite name render =
  Site
    { siteName = name,
      --siteRender =
      --  SiteRender $ \() () -> pure $ AssetGenerated Html render,
      siteModelManager =
        constModel ()
    }

constModel :: a -> ModelManager a r
constModel x = ModelManager $ \_ _ -> do
  let f _ = pure ()
  -- TODO: log it
  pure $ Dynamic (x, f)

instance Mergeable Site where
  merge site1 site2 =
    Site
      ((<>) (siteName site1) (siteName site2))
      (merge (siteModelManager site1) (siteModelManager site2))

class IsRoute r => RenderAsset r where
  renderAsset ::
    RouteEncoder (RouteModel r) r ->
    RouteModel r ->
    r ->
    Asset LByteString

instance (RenderAsset r1, RenderAsset r2, IsRoute (Either r1 r2), RouteModel (Either r1 r2) ~ (RouteModel r1, RouteModel r2)) => RenderAsset (Either r1 r2) where
  renderAsset enc m = \case
    Left r -> renderAsset @r1 (leftRouteEncoder enc) (fst m) r
    Right r -> renderAsset @r2 (rightRouteEncoder enc) (snd m) r

instance Mergeable ModelManager where
  merge r1 r2 = ModelManager $ \cliAct enc -> do
    x1 <- runModelManager r1 cliAct $ leftRouteEncoder enc
    x2 <- runModelManager r2 cliAct $ rightRouteEncoder enc
    pure $ (,) <$> x1 <*> x2
