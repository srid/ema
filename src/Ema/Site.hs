{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Site
  ( Site (..),

    -- * Creating sites
    singlePageSite,

    -- * Combinators
    mountUnder,
    mergeSite,
    (+:),
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Some (Some)
import Data.Text qualified as T
import Ema.Asset (Asset (AssetGenerated), Format (Html))
import Ema.CLI qualified as CLI
import Ema.Route
  ( PartialIsoEnumerableWithCtx,
    allRoutes,
    decodeRoute,
    encodeRoute,
  )
import System.FilePath ((</>))
import Text.Show (Show (show))
import UnliftIO (MonadUnliftIO, race, race_)

-- | A self-contained Ema site.
data Site r a = Site
  { siteRender ::
      Some CLI.Action ->
      PartialIsoEnumerableWithCtx a FilePath r ->
      a ->
      r ->
      Asset LByteString,
    -- | Thread that will patch the model over time.
    siteModelPatcher ::
      forall m b.
      (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
      Some CLI.Action ->
      -- Sets the initial mode, and takes a continuation to patch it.
      --
      -- The continuation will be called only on live server mode.
      (a -> (LVar a -> m ()) -> m b) ->
      m b,
    siteRouteEncoder ::
      PartialIsoEnumerableWithCtx a FilePath r
  }

-- | Create a site with a single 'index.html' route, who contents is specified
-- by the given function.
singlePageSite :: (Some CLI.Action -> LByteString) -> Site () ()
singlePageSite render =
  Site
    { siteRender = \act _enc () () -> AssetGenerated Html $ render act,
      siteModelPatcher = \_act setModel -> do
        setModel () (\(_ :: LVar.LVar ()) -> pure ()), -- pure ()),
      siteRouteEncoder =
        ( \() () -> "index.html",
          \_ fp -> guard (fp == "index.html"),
          \_ -> [()]
        )
    }

-- TODO: Using (Generic r)
-- simpleSite :: forall r enc. (enc -> r -> LByteString) -> Site r ()
-- simpleSite render = undefined

(+:) :: forall r1 r2 a1 a2. Site r1 a1 -> Site r2 a2 -> Site (Either r1 r2) (a1, a2)
(+:) = mergeSite

-- | Merge two sites to produce a single site.
mergeSite :: forall r1 r2 a1 a2. Site r1 a1 -> Site r2 a2 -> Site (Either r1 r2) (a1, a2)
mergeSite site1 site2 =
  Site render patch enc
  where
    render cliAct _ x = \case
      Left r -> siteRender site1 cliAct (siteRouteEncoder site1) (fst x) r
      Right r -> siteRender site2 cliAct (siteRouteEncoder site2) (snd x) r
    enc =
      ( \(a, b) -> \case
          Left r -> encodeRoute (siteRouteEncoder site1) a r
          Right r -> encodeRoute (siteRouteEncoder site2) b r,
        \(a, b) fp ->
          fmap Left (decodeRoute (siteRouteEncoder site1) a fp)
            <|> fmap Right (decodeRoute (siteRouteEncoder site2) b fp),
        \(a, b) ->
          fmap Left (allRoutes (siteRouteEncoder site1) a)
            <> fmap Right (allRoutes (siteRouteEncoder site2) b)
      )
    patch ::
      forall m b.
      (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
      Some CLI.Action ->
      ((a1, a2) -> (LVar (a1, a2) -> m ()) -> m b) ->
      m b
    patch cliAct f = do
      siteModelPatcher site1 cliAct $ \v1 k1 -> do
        l1 <- LVar.empty
        LVar.set l1 v1
        siteModelPatcher site2 cliAct $ \v2 k2 -> do
          l2 <- LVar.empty
          LVar.set l2 v2
          f (v1, v2) $ \lvar -> do
            race_
              (race_ (k1 l1) (k2 l2))
              ( do
                  sub1 <- LVar.addListener l1
                  sub2 <- LVar.addListener l2
                  forever $ do
                    x <-
                      race
                        (LVar.listenNext l1 sub1)
                        (LVar.listenNext l2 sub2)
                    case x of
                      Left a -> LVar.modify lvar $ \(_, b) -> (a, b)
                      Right b -> LVar.modify lvar $ \(a, _) -> (a, b)
              )

-- | Transform the given site such that all of its routes are encoded to be
-- under the given prefix.
mountUnder :: forall r a. String -> Site r a -> Site (PrefixedRoute r) a
mountUnder prefix Site {..} =
  Site siteRender' siteModelPatcher routeEncoder
  where
    siteRender' cliAct rEnc model (PrefixedRoute _ r) =
      siteRender cliAct (conv rEnc) model r
    conv ::
      PartialIsoEnumerableWithCtx a FilePath (PrefixedRoute r) ->
      PartialIsoEnumerableWithCtx a FilePath r
    conv (to, from, enum) =
      ( \m r -> to m (PrefixedRoute prefix r),
        \m fp -> _prefixedRouteRoute <$> from m fp,
        \m -> _prefixedRouteRoute <$> enum m
      )
    routeEncoder :: PartialIsoEnumerableWithCtx a FilePath (PrefixedRoute r)
    routeEncoder =
      let (to, from, all_) = siteRouteEncoder
       in ( \m r -> prefix </> to m (_prefixedRouteRoute r),
            \m fp -> do
              fp' <- fmap toString $ T.stripPrefix (toText $ prefix <> "/") $ toText fp
              fmap (PrefixedRoute prefix) . from m $ fp',
            fmap (PrefixedRoute prefix) . all_
          )

-- | A route that is prefixed at some URL prefix
data PrefixedRoute r = PrefixedRoute
  { _prefixedRoutePrefix :: String,
    _prefixedRouteRoute :: r
  }

instance (Show r) => Show (PrefixedRoute r) where
  show (PrefixedRoute p r) = p <> ":" <> Text.Show.show r
