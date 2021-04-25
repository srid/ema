{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Generate where

import Control.Exception (throw)
import Ema.Class
import Ema.Route (routeFile)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.FilePattern.Directory (getDirectoryFiles)

generate ::
  forall model route.
  Ema model route =>
  FilePath ->
  model ->
  (model -> route -> LByteString) ->
  IO ()
generate dest model render = do
  unlessM (doesDirectoryExist dest) $ do
    error "Destination does not exist"
  let routes = staticRoutes model
  putStrLn $ "Writing " <> show (length routes) <> " routes"
  forM_ routes $ \r -> do
    let fp = dest </> routeFile @model r
    putStrLn $ "W " <> fp
    let !s = render model r
    createDirectoryIfMissing True (takeDirectory fp)
    writeFileLBS fp s
  forM_ (staticAssets $ Proxy @route) $ \staticPath -> do
    copyDirRecursively staticPath dest

newtype StaticAssetMissing = StaticAssetMissing FilePath
  deriving (Show, Exception)

copyDirRecursively ::
  -- | Source file or directory relative to CWD that will be copied
  FilePath ->
  -- | Directory *under* which the source file/dir will be copied
  FilePath ->
  IO ()
copyDirRecursively srcRel destParent =
  doesFileExist srcRel >>= \case
    True -> do
      let b = destParent </> srcRel
      putStrLn $ "C " <> b
      copyFile srcRel b
    False ->
      doesDirectoryExist srcRel >>= \case
        False ->
          throw $ StaticAssetMissing srcRel
        True -> do
          fs <- getDirectoryFiles srcRel ["**"]
          forM_ fs $ \fp -> do
            let a = srcRel </> fp
                b = destParent </> srcRel </> fp
            putStrLn $ "C " <> b
            createDirectoryIfMissing True (takeDirectory b)
            copyFile a b
