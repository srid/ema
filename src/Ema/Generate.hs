{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Generate where

import Control.Exception (throw)
import Control.Monad.Logger
import Ema.Route (FileRoute, routeFile)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist)
import System.FilePath (takeDirectory, (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import UnliftIO (MonadUnliftIO)

log :: MonadLogger m => LogLevel -> Text -> m ()
log = logWithoutLoc "Generate"

generate ::
  forall model route m.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLoggerIO m,
    FileRoute route
  ) =>
  FilePath ->
  model ->
  [FilePath] ->
  [route] ->
  (model -> route -> LByteString) ->
  m ()
generate dest model staticAssets routes render = do
  unlessM (liftIO $ doesDirectoryExist dest) $ do
    error $ "Destination does not exist: " <> toText dest
  log LevelInfo $ "Writing " <> show (length routes) <> " routes"
  forM_ routes $ \r -> do
    let fp = dest </> routeFile r
    log LevelInfo $ toText $ "W " <> fp
    let !s = render model r
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory fp)
      writeFileLBS fp s
  forM_ staticAssets $ \staticPath -> do
    liftIO (doesPathExist staticPath) >>= \case
      True ->
        copyDirRecursively staticPath dest
      False ->
        log LevelWarn $ toText $ "? " <> staticPath <> " (missing)"

newtype StaticAssetMissing = StaticAssetMissing FilePath
  deriving (Show, Exception)

copyDirRecursively ::
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLoggerIO m
  ) =>
  -- | Source file or directory relative to CWD that will be copied
  FilePath ->
  -- | Directory *under* which the source file/dir will be copied
  FilePath ->
  m ()
copyDirRecursively srcRel destParent =
  liftIO (doesFileExist srcRel) >>= \case
    True -> do
      let b = destParent </> srcRel
      log LevelInfo $ toText $ "C " <> b
      liftIO $ copyFile srcRel b
    False ->
      liftIO (doesDirectoryExist srcRel) >>= \case
        False ->
          throw $ StaticAssetMissing srcRel
        True -> do
          fs <- liftIO $ getDirectoryFiles srcRel ["**"]
          forM_ fs $ \fp -> do
            let a = srcRel </> fp
                b = destParent </> srcRel </> fp
            log LevelInfo $ toText $ "C " <> b
            liftIO $ do
              createDirectoryIfMissing True (takeDirectory b)
              copyFile a b
