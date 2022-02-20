{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Site
  ( Site (..),

    -- * Creating sites
    singlePageSite,
    constModal,

    -- * Combinators
    mountUnder,
    mergeSite,
    (+:),
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
  ( RouteEncoder,
    leftRouteEncoder,
    mergeRouteEncoder,
    pimap,
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
    -- | Thread that will patch the model over time.
    siteModelData :: ModelRunner a r,
    siteRouteEncoder :: RouteEncoder a r
  }

type NonEmptyLVar m a =
  ( -- Initial value
    a,
    -- Generator for subsequent values
    LVar a -> m ()
  )

type ModelRunner a r =
  forall m.
  (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
  Some CLI.Action ->
  RouteEncoder a r ->
  m (NonEmptyLVar m a)

type SiteRender a r =
  RouteEncoder a r ->
  a ->
  r ->
  Asset LByteString

-- | Create a site with a single 'index.html' route, whose contents is specified
-- by the given function.
--
-- TODO: pass enc anyway.
singlePageSite :: SiteName -> LByteString -> Site () ()
singlePageSite name render =
  Site
    { siteName = name,
      siteRender =
        \_enc () () -> AssetGenerated Html render,
      siteModelData =
        constModal (),
      siteRouteEncoder =
        singletonRouteEncoder
    }

constModal :: a -> ModelRunner a r
constModal x _cliAct _site = do
  let f _ = pure ()
  -- TODO: log it
  pure (x, f)

(+:) :: forall r1 r2 a1 a2. Site a1 r1 -> Site a2 r2 -> Site (a1, a2) (Either r1 r2)
(+:) = mergeSite

-- | Class of product-cum-sum indexed types that can be merged.
class Mergeable (f :: Type -> Type -> Type) where
  -- | Merge by treating the first index as product, and second as sum.
  merge :: f a b -> f c d -> f (a, c) (Either b d)

-- | Merge two sites to produce a single site.
-- TODO: Avoid unnecessary updates on site1 webpage when only site2 changes (eg:
-- basic shouldn't refresh when clock changes)
mergeSite :: forall r1 r2 a1 a2. Site a1 r1 -> Site a2 r2 -> Site (a1, a2) (Either r1 r2)
mergeSite site1 site2 =
  Site
    (siteName site1 <> siteName site2)
    (mergeRender (siteRender site1) (siteRender site2))
    (mergeModelRunner (siteModelData site1) (siteModelData site2))
    (mergeRouteEncoder (siteRouteEncoder site1) (siteRouteEncoder site2))

mergeRender :: SiteRender a1 r1 -> SiteRender a2 r2 -> SiteRender (a1, a2) (Either r1 r2)
mergeRender r1 r2 enc x = \case
  Left r -> r1 (leftRouteEncoder enc) (fst x) r
  Right r -> r2 (rightRouteEncoder enc) (snd x) r

mergeModelRunner :: ModelRunner a1 r1 -> ModelRunner a2 r2 -> ModelRunner (a1, a2) (Either r1 r2)
mergeModelRunner r1 r2 cliAct enc' = do
  (v1, k1) <- r1 cliAct $ leftRouteEncoder enc'
  (v2, k2) <- r2 cliAct $ rightRouteEncoder enc'
  l1 <- LVar.empty
  l2 <- LVar.empty
  LVar.set l1 v1
  LVar.set l2 v2
  let k' lvar = do
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
  pure ((v1, v2), k')

-- | Transform the given site such that all of its routes are encoded to be
-- under the given prefix.
mountUnder :: forall r a. String -> Site a r -> Site a (PrefixedRoute r)
mountUnder prefix Site {..} =
  Site siteName siteRender' siteModelData' (toRouteEncoder siteRouteEncoder)
  where
    siteModelData' :: ModelRunner a (PrefixedRoute r)
    siteModelData' cliAct enc =
      siteModelData cliAct (fromRouteEncoder enc)
    siteRender' rEnc model r =
      siteRender (fromRouteEncoder rEnc) model (_prefixedRouteRoute r)
    toRouteEncoder :: RouteEncoder a r -> RouteEncoder a (PrefixedRoute r)
    toRouteEncoder =
      pimap
        (iso (prefix </>) $ fmap toString . T.stripPrefix (toText $ prefix <> "/") . toText)
        (iso (Just . PrefixedRoute prefix) _prefixedRouteRoute)
        id
    -- This coerces the r, but without losing the encoding.
    fromRouteEncoder :: RouteEncoder a (PrefixedRoute r) -> RouteEncoder a r
    fromRouteEncoder =
      pimap (iso id Just) (iso (Just . _prefixedRouteRoute) $ PrefixedRoute prefix) id

-- | A route that is prefixed at some URL prefix
data PrefixedRoute r = PrefixedRoute
  { _prefixedRoutePrefix :: String,
    _prefixedRouteRoute :: r
  }
  deriving stock (Eq, Ord)

instance (Show r) => Show (PrefixedRoute r) where
  show (PrefixedRoute p r) = p <> ":" <> Text.Show.show r
