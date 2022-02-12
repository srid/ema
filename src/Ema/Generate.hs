{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ema.Generate where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.LVar
import Data.LVar qualified as LVar
import Data.Some
import Ema.Asset (Asset (..))
import Ema.CLI qualified as CLI
import Ema.Class (Ema (ModelFor))
import Ema.Site
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist)
import System.FilePath (takeDirectory, (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import UnliftIO (MonadUnliftIO)
import UnliftIO.IO
  ( BufferMode (BlockBuffering, LineBuffering),
    hFlush,
    hSetBuffering,
  )

log :: MonadLogger m => LogLevel -> Text -> m ()
log = logWithoutLoc "Generate"

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
  let enc = siteRouteEncoder site
  withBlockBuffering $
    generate dest enc val (siteRender site cliAction enc)
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)

generate ::
  forall r m.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLoggerIO m,
    Ema r,
    HasCallStack
  ) =>
  FilePath ->
  PartialIsoEnumerableWithCtx (ModelFor r) FilePath r ->
  ModelFor r ->
  (ModelFor r -> r -> Asset LByteString) ->
  -- | List of generated files.
  m [FilePath]
generate dest (encodeRoute, _, allRoutes) model render = do
  unlessM (liftIO $ doesDirectoryExist dest) $ do
    error $ "Destination does not exist: " <> toText dest
  let routes = allRoutes model
  when (null routes) $
    error "allRoutes is empty; nothing to generate"
  log LevelInfo $ "Writing " <> show (length routes) <> " routes"
  let (staticPaths, generatedPaths) =
        lefts &&& rights $
          routes <&> \r ->
            case render model r of
              AssetStatic fp -> Left (r, fp)
              AssetGenerated _fmt s -> Right (encodeRoute model r, s)
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
        copyDirRecursively (encodeRoute model r) staticPath dest
      False ->
        log LevelWarn $ toText $ "? " <> staticPath <> " (missing)"
  noBirdbrainedJekyll dest
  pure paths

-- | Disable birdbrained hacks from GitHub to disable surprises like,
-- https://github.com/jekyll/jekyll/issues/55
noBirdbrainedJekyll :: (MonadIO m, MonadLoggerIO m) => FilePath -> m ()
noBirdbrainedJekyll dest = do
  let nojekyll = dest </> ".nojekyll"
  liftIO (doesFileExist nojekyll) >>= \case
    True -> pure ()
    False -> do
      log LevelInfo $ "Disabling Jekyll by writing " <> toText nojekyll
      writeFileLBS nojekyll ""

newtype StaticAssetMissing = StaticAssetMissing FilePath
  deriving stock (Show)
  deriving anyclass (Exception)

copyDirRecursively ::
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLoggerIO m,
    HasCallStack
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
          throw $ StaticAssetMissing srcAbs
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
