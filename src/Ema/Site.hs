{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Site
  ( Site (..),
    generateSite,
  )
where

import Control.Monad.Logger (MonadLoggerIO, logInfoN)
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Some (Some)
import Data.Text qualified as T
import Ema.Asset (Asset)
import Ema.CLI qualified as CLI
import Ema.Class
import Ema.Generate qualified as Generate
import GHC.TypeLits
import System.FilePath
import UnliftIO
  ( MonadUnliftIO,
  )
import UnliftIO.IO
  ( BufferMode (BlockBuffering, LineBuffering),
    hFlush,
    hSetBuffering,
  )
import Prelude

-- | An Iso that is not necessarily surjective; as well as takes an (unchanging)
-- context value.
type PartialIsoEnumerableWithCtx ctx s a = (ctx -> a -> s, ctx -> s -> Maybe a, ctx -> [a])

partialIsoIsLawfulForCtx :: Eq a => PartialIsoEnumerableWithCtx ctx s a -> ctx -> Bool
partialIsoIsLawfulForCtx (to, from, getas) ctx =
  all (\a -> let s = to ctx a in Just a == from ctx s) (getas ctx)

data Site r = Site
  { siteRender ::
      Some CLI.Action ->
      ModelFor r ->
      r ->
      Asset LByteString,
    siteRunModel ::
      forall m.
      (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
      Some CLI.Action ->
      ((ModelFor r -> ModelFor r) -> m ()) ->
      m ()
  }

data RoutePrefix (p :: Symbol) r = RoutePrefix r

type NoteRoute = ()

type NoteRouteMouted = RoutePrefix "notes" NoteRoute

ex :: Site r -> Site (RoutePrefix "foo" r)
ex s = siteUnder @"foo" s

siteUnder :: forall p r. Site r -> Site (RoutePrefix p r)
siteUnder Site {..} =
  Site siteRender' siteRunModel
  where
    siteRender' cliAct model (RoutePrefix r) =
      siteRender cliAct model r

instance (Ema r, KnownSymbol p) => Ema (RoutePrefix p r) where
  type ModelFor (RoutePrefix p r) = ModelFor r
  encodeRoute m (RoutePrefix r) =
    symbolVal (Proxy @p) </> encodeRoute @r m r
  decodeRoute m fp = do
    fp' <- fmap toString $ T.stripPrefix (toText $ symbolVal (Proxy @p) <> "/") $ toText fp
    RoutePrefix <$> decodeRoute @r m fp'
  allRoutes =
    fmap RoutePrefix . allRoutes @r

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
        forkIO $ siteRunModel site cliAct (siteData site)
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

generateSite ::
  forall m r.
  (MonadIO m, MonadUnliftIO m, MonadLoggerIO m, Ema r) =>
  Some CLI.Action ->
  FilePath ->
  Site r ->
  LVar (ModelFor r) ->
  m [FilePath]
generateSite cliAction dest site model = do
  val :: ModelFor r <- LVar.get model
  logInfoN "... initial model is now available."
  withBlockBuffering $
    Generate.generate dest val (siteRender site cliAction)
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)

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
