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
    mergeRouteEncoder,
    pimap,
    singletonRouteEncoder,
  )
import System.FilePath ((</>))
import Text.Show (Show (show))
import UnliftIO (MonadUnliftIO, race, race_)
import UnliftIO.Concurrent (threadDelay)

-- | A self-contained Ema site.
data Site a r = Site
  { siteName :: Text,
    siteRender ::
      Some CLI.Action ->
      RouteEncoder a r ->
      a ->
      r ->
      Asset LByteString,
    -- | Thread that will patch the model over time.
    siteModelData :: ModelRunner a,
    siteRouteEncoder :: RouteEncoder a r
  }

type NonEmptyLVar m a =
  ( -- Initial value
    a,
    -- Generator for subsequent values
    LVar a -> m ()
  )

type ModelRunner a =
  forall m.
  (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
  Some CLI.Action ->
  m (NonEmptyLVar m a)

-- | Create a site with a single 'index.html' route, whose contents is specified
-- by the given function.
--
-- TODO: pass enc anyway.
singlePageSite :: Text -> (Some CLI.Action -> LByteString) -> Site () ()
singlePageSite name render =
  Site
    { siteName = name,
      siteRender =
        \act _enc () () -> AssetGenerated Html $ render act,
      siteModelData =
        constModal (),
      siteRouteEncoder =
        singletonRouteEncoder
    }

constModal :: a -> ModelRunner a
constModal x _cliAct = do
  let f _ = pure ()
  -- TODO: log it
  pure (x, f)

(+:) :: forall r1 r2 a1 a2. Site a1 r1 -> Site a2 r2 -> Site (a1, a2) (Either r1 r2)
(+:) = mergeSite

class Mergeable (f :: Type -> Type -> Type) where
  merge :: f a b -> f c d -> f (a, c) (Either b d)

-- | Merge two sites to produce a single site.
-- TODO: Avoid unnecessary updates on site1 webpage when only site2 changes (eg:
-- basic shouldn't refresh when clock changes)
mergeSite :: forall r1 r2 a1 a2. Site a1 r1 -> Site a2 r2 -> Site (a1, a2) (Either r1 r2)
mergeSite site1 site2 =
  Site name render (runBoth (siteModelData site1) (siteModelData site2)) enc
  where
    name = siteName site1 <> "+" <> siteName site2
    enc = mergeRouteEncoder enc1 enc2
    enc1 = siteRouteEncoder site1
    enc2 = siteRouteEncoder site2
    render cliAct _ x = \case
      Left r -> siteRender site1 cliAct enc1 (fst x) r
      Right r -> siteRender site2 cliAct enc2 (snd x) r
    runBoth :: ModelRunner a1 -> ModelRunner a2 -> ModelRunner (a1, a2)
    runBoth r1 r2 cliAct = do
      (v1, k1) <- r1 cliAct
      (v2, k2) <- r2 cliAct
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
                  (k1 l1 >> keepAlive (siteName site1))
                  (k2 l2 >> keepAlive (siteName site2))
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
  Site siteName siteRender' siteModelData (toRouteEncoder siteRouteEncoder)
  where
    siteRender' cliAct rEnc model r =
      siteRender cliAct (fromRouteEncoder rEnc) model (_prefixedRouteRoute r)
    toRouteEncoder :: RouteEncoder a r -> RouteEncoder a (PrefixedRoute r)
    toRouteEncoder =
      pimap
        (iso (prefix </>) $ fmap toString . T.stripPrefix (toText $ prefix <> "/") . toText)
        (iso (PrefixedRoute prefix) _prefixedRouteRoute)
        id
    fromRouteEncoder :: RouteEncoder a (PrefixedRoute r) -> RouteEncoder a r
    fromRouteEncoder =
      pimap (iso id Just) (iso _prefixedRouteRoute $ PrefixedRoute prefix) id

-- | A route that is prefixed at some URL prefix
data PrefixedRoute r = PrefixedRoute
  { _prefixedRoutePrefix :: String,
    _prefixedRouteRoute :: r
  }
  deriving stock (Eq, Ord)

instance (Show r) => Show (PrefixedRoute r) where
  show (PrefixedRoute p r) = p <> ":" <> Text.Show.show r
