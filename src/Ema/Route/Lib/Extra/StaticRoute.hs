{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use impureThrow" #-}

module Ema.Route.Lib.Extra.StaticRoute (
  StaticRoute,
  Model (..),

  -- * Helpers
  staticRouteUrl,
) where

import Control.Exception (throw)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Some (Some)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Ema
import Ema.CLI qualified
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Optics.Core (Prism', prism')
import System.FilePath (takeExtension, (</>))
import System.UnionMount qualified as UnionMount
import UnliftIO (MonadUnliftIO)

-- | Route to a static file under @baseDir@.
newtype StaticRoute (baseDir :: Symbol) = StaticRoute {unStaticRoute :: FilePath}
  deriving newtype (Eq, Ord, Show)
  deriving stock (Generic)

data Model = Model
  { modelCliAction :: Some Ema.CLI.Action
  , modelFiles :: Map FilePath UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance IsRoute (StaticRoute baseDir) where
  type RouteModel (StaticRoute baseDir) = Model
  routePrism (modelFiles -> files) =
    let enc =
          unStaticRoute
        dec fp =
          StaticRoute fp <$ guard (Map.member fp files)
     in toPrism_ $ prism' enc dec
  routeUniverse (modelFiles -> files) =
    StaticRoute <$> Map.keys files

instance KnownSymbol baseDir => EmaSite (StaticRoute baseDir) where
  siteInput cliAct _ = do
    files <- staticFilesDynamic $ symbolVal (Proxy @baseDir)
    pure $ Model cliAct <$> files
  siteOutput _ _ (StaticRoute path) =
    pure $ Ema.AssetStatic $ symbolVal (Proxy @baseDir) </> path

staticFilesDynamic ::
  forall m.
  (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m) =>
  FilePath ->
  m (Dynamic m (Map FilePath UTCTime))
staticFilesDynamic baseDir = do
  let pats = [((), "**")]
      ignorePats = [".*"]
      model0 = mempty
  Dynamic <$> UnionMount.mount baseDir pats ignorePats model0 (const handleUpdate)
  where
    handleUpdate ::
      FilePath ->
      UnionMount.FileAction () ->
      m (Map FilePath UTCTime -> Map FilePath UTCTime)
    handleUpdate fp = \case
      UnionMount.Refresh _ _ -> do
        lastAccessed <- liftIO getCurrentTime
        pure $ Map.insert fp lastAccessed
      UnionMount.Delete -> do
        pure $ Map.delete fp

-- | Like `Ema.routeUrl`, but looks up the value and appends it to URL in live-server (for force-reload in browser)
staticRouteUrl ::
  forall s baseDir staticRoute.
  (IsString s, staticRoute ~ StaticRoute baseDir) =>
  Prism' FilePath staticRoute ->
  RouteModel staticRoute ->
  FilePath ->
  s
staticRouteUrl rp model fp =
  let lastAccessed = lookupMust fp model
      tag = toText $ formatTime defaultTimeLocale "%s" lastAccessed
      url = Ema.routeUrl rp $ StaticRoute fp
   in fromString . toString $ url <> refreshAddendum tag
  where
    -- Force the browser to reload the static file referenced
    refreshAddendum tag =
      fromMaybe "" $ do
        -- In live server, force reload all (re-added/modified) static files.
        -- In statically generated site, do it only for CSS and JS files.
        guard $
          Ema.CLI.isLiveServer (modelCliAction model)
            || takeExtension fp `List.elem` [".css", ".js"]
        pure $ "?" <> tag

lookupMust :: FilePath -> Model -> UTCTime
lookupMust fp model =
  case Map.lookup fp (modelFiles model) of
    Just lastAccessed -> lastAccessed
    Nothing -> throw $ MissingStaticFile fp

newtype MissingStaticFile = MissingStaticFile FilePath
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
