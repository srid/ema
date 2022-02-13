{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Site
  ( Site (..),
    PartialIsoEnumerableWithCtx,
    defaultEnum,
    siteUnder,
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Some (Some)
import Data.Text qualified as T
import Ema.Asset (Asset)
import Ema.CLI qualified as CLI
import GHC.TypeLits
import System.FilePath
import Text.Show (Show (show))
import UnliftIO
  ( MonadUnliftIO,
    race,
    race_,
  )
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

eitherSites :: forall r1 r2 a1 a2. Site r1 a1 -> Site r2 a2 -> Site (Either r1 r2) (a1, a2)
eitherSites site1 site2 =
  Site render patch enc
  where
    render cliAct rEnc x = \case
      Left r -> siteRender site1 cliAct (siteRouteEncoder site1) (fst x) r
      Right r -> siteRender site2 cliAct (siteRouteEncoder site2) (snd x) r
    enc = undefined
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

{-
type MultiSite rs = (NS I rs)

mkMultiSite :: forall m models mx ms. (
    models ~ (mx ': ms),
    Data.SOP.All Ema models,
    MonadIO m,
    MonadUnliftIO  m,
    MonadLoggerIO m,
    SelectSum I RouteFor' models, SelectSum Site RouteFor' models, SelectSum2 I Site RouteFor' models) =>  NP Site models -> m (Site (MultiSite models))
mkMultiSite sites = do
  model <- LVar.empty
  pure $ Site model run render listenNext
  where
    render cliAct (models :: NP I models) (route :: NS RouteFor' models) =
      selectSum2 models sites route $ \model site (RouteFor' r) ->
        siteRender site cliAct (unI model) r
    run :: forall m1. (MonadIO m1, MonadUnliftIO m1, MonadLoggerIO m1) => Some CLI.Action -> LVar (NP I models) -> m1 ()
    run cliAct (modelLvar :: LVar (NP I models)) = do
      models <- LVar.get modelLvar
      void $ sequence $ collapseSites sites $ \site -> do
        -- TODO: no fork
        forkIO $ siteModelPatcher site cliAct (siteData site)
        forkIO $ do
          subId <- LVar.addListener $ siteData site
          forever $ do
            val <- LVar.listenNext (siteData site) subId
            -- TODO: update models with val
            pure ()
          pure ()
        pure ()
    listenNext lvar subId =
      undefined

instance (rs ~ (mx ': ms), SelectNP ModelFor' ms, SelectSum I ModelFor' ms, Data.SOP.All Ema rs) => Ema (MultiSite rs) where
  type ModelFor (MultiSite rs) = NP ModelFor' rs
  decodeRoute sites fp = do
    let routes :: NP (Compose Maybe RouteFor') models = hcmap (Proxy :: Proxy Ema) (\site -> Compose $ fmap RouteFor' $ decodeRoute (unI site) fp) sites
    selectNP routes
  encodeRoute sites r =
    selectSum sites r $ \(model :: I model) (RouteFor' r :: RouteFor' model) ->
      encodeRoute (unI model) r
  allRoutes sites =
    -- collapseSites sites $ \site ->
    --  allRoutes sites
    undefined

data ModelFor' r = ModelFor' (ModelFor r)

class SelectSum f g as where
  selectSum :: NP f as -> NS g as -> (forall x. Ema x => f x -> g x -> r) -> r

instance Ema x => SelectSum f g (x ': '[]) where
  selectSum (p :* Nil) (Z x) f = f p x

instance SelectSum f g xs => SelectSum f g (x ': xs) where
  selectSum (p :* ps) (S x) f = selectSum ps x f

class SelectSum2 f g h as where
  selectSum2 :: NP f as -> NP g as -> NS h as -> (forall x. Ema x => f x -> g x -> h x -> r) -> r

instance Ema x => SelectSum2 f g h (x ': '[]) where
  selectSum2 (p1 :* Nil) (p2 :* Nil) (Z x) f = f p1 p2 x

instance SelectSum2 f g h xs => SelectSum2 f g h (x ': xs) where
  selectSum2 (p :* ps) (q :* qs) (S x) f = selectSum2 ps qs x f

class SelectNP  f as where
  selectNP :: NP (Compose Maybe f) as -> Maybe (NS f as)

instance SelectNP f '[] where
  selectNP Nil = Nothing

instance SelectNP f xs => SelectNP f (x ': xs) where
  selectNP (p1 :* prest) =
    case getCompose p1 of
      Nothing -> S <$> selectNP prest
      Just v -> Just $ Z v

-}

{-
generateSites ::
  forall (models :: [Type]) m mx ms.
  (MonadIO m, MonadUnliftIO m, MonadLoggerIO m, Data.SOP.All Ema models, models ~ (mx ': ms)) =>
  Some CLI.Action ->
  FilePath ->
  NP Site models ->
  m [FilePath]
generateSites cliAction dest sites =
  fmap concat $ sequence $ collapseSites sites (generateSite cliAction dest)

collapseSites ::
  forall (models :: [Type]) r f mx ms.
  (Data.SOP.All Ema models, models ~ (mx ': ms)) =>
  NP f models ->
  (forall model. Ema model => f model -> r) ->
  [r]
collapseSites sites f =
  let genSites :: NP (K r) models = hcmap (Proxy :: Proxy Ema) (K . f) sites
      res :: [r] = hcollapse genSites
   in res

-}
