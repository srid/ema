{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ema.Generate (generateSite) where

import Control.Exception (throwIO)
import Control.Monad.Logger
import Ema.Asset
import Ema.Route.Class (IsRoute (RouteModel, allRoutes, routeEncoder))
import Ema.Route.Encoder
import Ema.Site
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist)
import System.FilePath (takeDirectory, (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import UnliftIO (MonadUnliftIO)

log :: MonadLogger m => LogLevel -> Text -> m ()
log = logWithoutLoc "Generate"

generateSite ::
  forall r m.
  (MonadIO m, MonadUnliftIO m, MonadLoggerIO m, Eq r, Show r, IsRoute r, EmaSite r) =>
  FilePath ->
  RouteModel r ->
  m [FilePath]
generateSite dest model = do
  let enc = routeEncoder @r
  -- cliAct = Some $ CLI.Generate dest
  withBlockBuffering $
    generate dest enc model (siteOutput enc)
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)

generate ::
  forall r m.
  ( MonadIO m
  , MonadUnliftIO m
  , MonadLoggerIO m
  , HasCallStack
  , Eq r
  , Show r
  , EmaSite r
  ) =>
  FilePath ->
  RouteEncoder (RouteModel r) r ->
  RouteModel r ->
  (RouteModel r -> r -> Asset LByteString) ->
  -- | List of generated files.
  m [FilePath]
generate dest enc model render = do
  unlessM (liftIO $ doesDirectoryExist dest) $ do
    error $ "Destination does not exist: " <> toText dest
  let routes = allRoutes @r model
  when (null routes) $
    error "allRoutes is empty; nothing to generate"
  forM_ routes $ \route ->
    case checkRouteEncoderGivenRoute enc model route of
      Left s -> error s
      Right () -> pass
  log LevelInfo $ "Writing " <> show (length routes) <> " routes"
  let (staticPaths, generatedPaths) =
        lefts &&& rights $
          routes <&> \r ->
            case render model r of
              AssetStatic fp -> Left (r, fp)
              AssetGenerated _fmt s -> Right (encodeRoute enc model r, s)
  paths <- forM generatedPaths $ \(relPath, !s) -> do
    let fp = dest </> relPath
    log LevelInfo $ toText $ "W " <> fp
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory fp)
      writeFileLBS fp s
      pure fp
  forM_ staticPaths $ \(r, staticPath) -> do
    liftIO (doesPathExist staticPath) >>= \case
      True ->
        -- TODO: In current branch, we don't expect this to be a directory.
        -- Although the user may pass it, but review before merge.
        copyDirRecursively (encodeRoute enc model r) staticPath dest
      False ->
        log LevelWarn $ toText $ "? " <> staticPath <> " (missing)"
  noBirdbrainedJekyll dest
  pure paths

{- | Disable birdbrained hacks from GitHub to disable surprises like,
 https://github.com/jekyll/jekyll/issues/55
-}
noBirdbrainedJekyll :: (MonadIO m, MonadLoggerIO m) => FilePath -> m ()
noBirdbrainedJekyll dest = do
  let nojekyll = dest </> ".nojekyll"
  liftIO (doesFileExist nojekyll) >>= \case
    True -> pass
    False -> do
      log LevelInfo $ "Disabling Jekyll by writing " <> toText nojekyll
      writeFileLBS nojekyll ""

newtype StaticAssetMissing = StaticAssetMissing FilePath
  deriving stock (Show)
  deriving anyclass (Exception)

copyDirRecursively ::
  ( MonadIO m
  , MonadUnliftIO m
  , MonadLoggerIO m
  , HasCallStack
  ) =>
  -- | Source file path relative to CWD
  FilePath ->
  -- | Absolute path to source file to copy.
  FilePath ->
  -- | Directory *under* which the source file/dir will be copied
  FilePath ->
  m ()
copyDirRecursively srcRel srcAbs destParent = do
  liftIO (doesFileExist srcAbs) >>= \case
    True -> do
      let b = destParent </> srcRel
      log LevelInfo $ toText $ "C " <> b
      copyFileCreatingParents srcAbs b
    False ->
      liftIO (doesDirectoryExist srcAbs) >>= \case
        False ->
          liftIO $ throwIO $ StaticAssetMissing srcAbs
        True -> do
          fs <- liftIO $ getDirectoryFiles srcAbs ["**"]
          forM_ fs $ \fp -> do
            let a = srcAbs </> fp
                b = destParent </> srcRel </> fp
            log LevelInfo $ toText $ "C " <> b
            copyFileCreatingParents a b
  where
    copyFileCreatingParents a b =
      liftIO $ do
        createDirectoryIfMissing True (takeDirectory b)
        copyFile a b
