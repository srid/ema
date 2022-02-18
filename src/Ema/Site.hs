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
      RouteEncoder r a ->
      a ->
      r ->
      Asset LByteString,
    -- | Thread that will patch the model over time.
    siteModelRunner :: ModelRunner a,
    siteRouteEncoder :: RouteEncoder r a
  }

-- | A process that knows how to initialize and update `LVar a` over time, and return `r` if it terminates.
type LVarRunner m a r =
  ( -- Initial value for the LVar
    a ->
    -- How to update the LVar over time.
    (LVar a -> m ()) ->
    m r
  )

type ModelRunner a =
  forall m r.
  (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
  Some CLI.Action ->
  LVarRunner m a r ->
  m r

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
      siteModelRunner =
        constModal (),
      siteRouteEncoder =
        singletonRouteEncoder
    }

constModal :: a -> ModelRunner a
constModal x _ startModel = do
  startModel x $ \_ ->
    -- TODO: log it
    pure ()

-- Create a site that using routes determined statically at compile time.
-- TODO: Implement this using (Generic r)
-- staticRouteSite :: forall r enc. (enc -> r -> LByteString) -> Site r ()
-- staticRouteSite render = undefined

(+:) :: forall r1 r2 a1 a2. Site a1 r1 -> Site a2 r2 -> Site (a1, a2) (Either r1 r2)
(+:) = mergeSite

-- | Merge two sites to produce a single site.
-- TODO: Avoid unnecessary updates on site1 webpage when only site2 changes (eg:
-- basic shouldn't refresh when clock changes)
mergeSite :: forall r1 r2 a1 a2. Site a1 r1 -> Site a2 r2 -> Site (a1, a2) (Either r1 r2)
mergeSite site1 site2 =
  Site name render (runBoth (siteModelRunner site1) (siteModelRunner site2)) enc
  where
    name = siteName site1 <> "+" <> siteName site2
    enc = mergeRouteEncoder (siteRouteEncoder site1) (siteRouteEncoder site2)
    render cliAct _ x = \case
      Left r -> siteRender site1 cliAct (siteRouteEncoder site1) (fst x) r
      Right r -> siteRender site2 cliAct (siteRouteEncoder site2) (snd x) r
    runBoth :: ModelRunner a1 -> ModelRunner a2 -> ModelRunner (a1, a2)
    runBoth r1 r2 cliAct f = do
      r1 cliAct $ \v1 k1 -> do
        l1 <- LVar.empty
        LVar.set l1 v1
        r2 cliAct $ \v2 k2 -> do
          l2 <- LVar.empty
          LVar.set l2 v2
          f (v1, v2) $ \lvar -> do
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

-- | Transform the given site such that all of its routes are encoded to be
-- under the given prefix.
mountUnder :: forall r a. String -> Site a r -> Site a (PrefixedRoute r)
mountUnder prefix Site {..} =
  Site siteName siteRender' siteModelRunner (toRouteEncoder siteRouteEncoder)
  where
    siteRender' cliAct rEnc model r =
      siteRender cliAct (fromRouteEncoder rEnc) model (_prefixedRouteRoute r)
    toRouteEncoder :: RouteEncoder r a -> RouteEncoder (PrefixedRoute r) a
    toRouteEncoder =
      pimap
        (iso (prefix </>) $ fmap toString . T.stripPrefix (toText $ prefix <> "/") . toText)
        (iso (PrefixedRoute prefix) _prefixedRouteRoute)
        id
    fromRouteEncoder :: RouteEncoder (PrefixedRoute r) a -> RouteEncoder r a
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
