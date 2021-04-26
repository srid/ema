{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Generate where

import Control.Exception (throw)
import Control.Monad.Logger
import Ema.Class
import Ema.Route (routeFile)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.FilePattern.Directory (getDirectoryFiles)

generate ::
  forall model route m.
  (MonadEma m, Ema model route) =>
  FilePath ->
  model ->
  (model -> route -> LByteString) ->
  m ()
generate dest model render = do
  unlessM (liftIO $ doesDirectoryExist dest) $ do
    error "Destination does not exist"
  let routes = staticRoutes model
  logInfoN $ "Writing " <> show (length routes) <> " routes"
  forM_ routes $ \r -> do
    let fp = dest </> routeFile @model r
    logInfoN $ toText $ "W " <> fp
    let !s = render model r
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory fp)
      writeFileLBS fp s
  forM_ (staticAssets $ Proxy @route) $ \staticPath -> do
    copyDirRecursively staticPath dest

newtype StaticAssetMissing = StaticAssetMissing FilePath
  deriving (Show, Exception)

copyDirRecursively ::
  MonadEma m =>
  -- | Source file or directory relative to CWD that will be copied
  FilePath ->
  -- | Directory *under* which the source file/dir will be copied
  FilePath ->
  m ()
copyDirRecursively srcRel destParent =
  liftIO (doesFileExist srcRel) >>= \case
    True -> do
      let b = destParent </> srcRel
      logInfoN $ toText $ "C " <> b
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
            logInfoN $ toText $ "C " <> b
            liftIO $ do
              createDirectoryIfMissing True (takeDirectory b)
              copyFile a b
