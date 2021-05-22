{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Generate where

import Control.Exception (throw)
import Control.Monad.Logger
import Ema.Asset
import Ema.Class
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
    Ema model route
  ) =>
  FilePath ->
  model ->
  (model -> route -> Asset LByteString) ->
  m ()
generate dest model render = do
  unlessM (liftIO $ doesDirectoryExist dest) $ do
    error $ "Destination does not exist: " <> toText dest
  let routes = allRoutes model
  log LevelInfo $ "Writing " <> show (length routes) <> " routes"
  let (staticPaths, generatedPaths) =
        lefts &&& rights $
          routes <&> \r ->
            case render model r of
              AssetStatic fp -> Left (r, fp)
              AssetGenerated _fmt s -> Right (encodeRoute r, s)
  forM_ generatedPaths $ \(relPath, !s) -> do
    let fp = dest </> relPath
    log LevelInfo $ toText $ "W " <> fp
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory fp)
      writeFileLBS fp s
  forM_ staticPaths $ \(_, staticPath) -> do
    liftIO (doesPathExist staticPath) >>= \case
      True ->
        -- TODO: In current branch, we don't expect this to be a directory.
        -- Although the user may pass it, but review before merge.
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
