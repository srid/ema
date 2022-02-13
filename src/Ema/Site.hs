{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Site
  ( Site (..),
    PartialIsoEnumerableWithCtx,
    defaultEnum,
    siteUnder,
    mergeSite,
    (+:),
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Some (Some)
import Data.Text qualified as T
import Ema.Asset (Asset)
import Ema.CLI qualified as CLI
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.FilePath (FilePath, (</>))
import Text.Show (Show (show))
import UnliftIO (MonadUnliftIO, race, race_)
import Prelude

-- | An Iso that is not necessarily surjective; as well as takes an (unchanging)
-- context value.
type PartialIsoEnumerableWithCtx ctx s a = (ctx -> a -> s, ctx -> s -> Maybe a, ctx -> [a])

partialIsoIsLawfulForCtx :: Eq a => PartialIsoEnumerableWithCtx ctx s a -> ctx -> Bool
partialIsoIsLawfulForCtx (to, from, getas) ctx =
  all (\a -> let s = to ctx a in Just a == from ctx s) (getas ctx)

defaultEnum :: (Bounded r, Enum r) => [r]
defaultEnum = [minBound .. maxBound]

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

-- TODO: Using (Generic r)
simpleSite :: forall r enc. (enc -> r -> LByteString) -> Site r ()
simpleSite render = undefined

encodeRoute :: Site r a -> a -> r -> FilePath
encodeRoute site m r =
  let (f, _, _) = siteRouteEncoder site
   in f m r

decodeRoute :: Site r a -> a -> FilePath -> Maybe r
decodeRoute site m fp =
  let (_, f, _) = siteRouteEncoder site
   in f m fp

allRoutes :: Site r a -> a -> [r]
allRoutes site m =
  let (_, _, f) = siteRouteEncoder site
   in f m

(+:) :: forall r1 r2 a1 a2. Site r1 a1 -> Site r2 a2 -> Site (Either r1 r2) (a1, a2)
(+:) = mergeSite

mergeSite :: forall r1 r2 a1 a2. Site r1 a1 -> Site r2 a2 -> Site (Either r1 r2) (a1, a2)
mergeSite site1 site2 =
  Site render patch enc
  where
    render cliAct _ x = \case
      Left r -> siteRender site1 cliAct (siteRouteEncoder site1) (fst x) r
      Right r -> siteRender site2 cliAct (siteRouteEncoder site2) (snd x) r
    enc =
      ( \(a, b) -> \case
          Left r -> encodeRoute site1 a r
          Right r -> encodeRoute site2 b r,
        \(a, b) fp ->
          fmap Left (decodeRoute site1 a fp)
            <|> fmap Right (decodeRoute site2 b fp),
        \(a, b) ->
          fmap Left (allRoutes site1 a)
            <> fmap Right (allRoutes site2 b)
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

-- TODO: Change to `PrefixedRoute String r`
data RoutePrefix (p :: Symbol) r = RoutePrefix {unRoutePrefix :: r}

instance (Show r, KnownSymbol p) => Show (RoutePrefix p r) where
  show (RoutePrefix r) = symbolVal (Proxy @p) <> ":" <> Text.Show.show r

siteUnder :: forall p r a. (KnownSymbol p) => Site r a -> Site (RoutePrefix p r) a
siteUnder Site {..} =
  Site siteRender' siteModelPatcher routeEncoder
  where
    siteRender' cliAct rEnc model (RoutePrefix r) =
      siteRender cliAct (conv rEnc) model r
    conv ::
      PartialIsoEnumerableWithCtx a FilePath (RoutePrefix p r) ->
      PartialIsoEnumerableWithCtx a FilePath r
    conv (to, from, enum) =
      ( \m r -> to m (RoutePrefix r),
        \m fp -> unRoutePrefix <$> from m fp,
        \m -> unRoutePrefix <$> enum m
      )
    routeEncoder :: PartialIsoEnumerableWithCtx a FilePath (RoutePrefix p r)
    routeEncoder =
      let (to, from, all_) = siteRouteEncoder
       in ( (\m r -> prefix </> to m (unRoutePrefix r)),
            ( \m fp -> do
                fp' <- fmap toString $ T.stripPrefix (toText $ symbolVal (Proxy @p) <> "/") $ toText fp
                fmap RoutePrefix . from m $ fp'
            ),
            (fmap RoutePrefix . all_)
          )
    prefix = symbolVal (Proxy @p)
