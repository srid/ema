{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use impureThrow" #-}

module Ema.Route.Lib.Extra.StaticRoute (
  StaticRoute,

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
newtype StaticRoute (baseDir :: Symbol) (a :: Type) = StaticRoute {unStaticRoute :: FilePath}
  deriving newtype (Eq, Ord, Show)

instance IsRoute (StaticRoute baseDir a) where
  type RouteModel (StaticRoute baseDir a) = Map FilePath a
  routeEncoder = Ema.mkRouteEncoder $ \files ->
    let enc =
          unStaticRoute
        dec fp =
          StaticRoute fp <$ guard (Map.member fp files)
     in prism' enc dec
  allRoutes files =
    StaticRoute <$> Map.keys files

instance KnownSymbol baseDir => EmaSite (StaticRoute baseDir UTCTime) where
  siteInput _ _ = do
    staticFilesDynamic $ symbolVal (Proxy @baseDir)
  siteOutput _ _ (StaticRoute path) =
    Ema.AssetStatic $ symbolVal (Proxy @baseDir) </> path

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
  forall a baseDir staticRoute.
  (IsString a, staticRoute ~ StaticRoute baseDir UTCTime) =>
  Some Ema.CLI.Action ->
  Prism' FilePath staticRoute ->
  RouteModel staticRoute ->
  FilePath ->
  a
staticRouteUrl cliAct rp model fp =
  case Map.lookup fp model of
    Just lastAccessed ->
      let tag = toText $ formatTime defaultTimeLocale "%s" lastAccessed
          url = Ema.routeUrl rp (StaticRoute fp)
       in fromString . toString $ url <> refreshAddendum tag
    Nothing -> throw $ MissingStaticFile fp
  where
    -- Force the browser to reload the static file referenced
    refreshAddendum tag =
      fromMaybe "" $ do
        -- In live server, force reload all (re-added/modified) static files.
        -- In statically generated site, do it only for CSS and JS files.
        guard $ Ema.CLI.isLiveServer cliAct || takeExtension fp `List.elem` [".css", ".js"]
        pure $ "?" <> tag

newtype MissingStaticFile = MissingStaticFile FilePath
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
