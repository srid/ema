{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ema.Generate (
  generateSiteFromModel,
  generateSiteFromModel',
) where

import Control.Exception (throwIO)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Logger (
  LogLevel (LevelError, LevelInfo),
  MonadLogger,
  MonadLoggerIO,
  logWithoutLoc,
 )
import Ema.Asset (Asset (..))
import Ema.CLI (crash)
import Ema.Route.Class (IsRoute (RouteModel, routeEncoder, routeUniverse))
import Ema.Route.Encoder (
  applyRouteEncoder,
  checkRouteEncoderGivenRoute,
 )
import Ema.Site (EmaSite (siteOutput), EmaStaticSite)
import Optics.Core (review)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist)
import System.FilePath (takeDirectory, (</>))
import System.FilePattern.Directory (getDirectoryFiles)

log :: MonadLogger m => LogLevel -> Text -> m ()
log = logWithoutLoc "ema.generate"

{- | Generate the static site at `dest`

  The *only* data we need is the `RouteModel`.
-}
generateSiteFromModel ::
  forall r m.
  (MonadIO m, MonadLoggerIO m, MonadFail m, Eq r, Show r, IsRoute r, EmaStaticSite r) =>
  -- | Target directory to write files to. Must exist.
  FilePath ->
  -- | The model data used to generate assets.
  RouteModel r ->
  m [FilePath]
generateSiteFromModel dest model =
  withBlockBuffering $ do
    runExceptT (generateSiteFromModel' @r dest model) >>= \case
      Left err -> do
        crash "ema" err
      Right fs ->
        pure fs
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)

-- | Like `generateSiteFromModel` but without buffering or error handling.
generateSiteFromModel' ::
  forall r m.
  (MonadIO m, MonadLoggerIO m, MonadError Text m, Eq r, Show r, EmaStaticSite r) =>
  FilePath ->
  RouteModel r ->
  -- | List of generated files.
  m [FilePath]
generateSiteFromModel' dest model = do
  let enc = routeEncoder @r
      rp = applyRouteEncoder enc model
  -- Sanity checks
  unlessM (liftIO $ doesDirectoryExist dest) $ do
    throwError $ "Destination directory does not exist: " <> toText dest
  let routes = routeUniverse @r model
  when (null routes) $
    throwError "Your app's `routeUniverse` is empty; nothing to generate!"
  forM_ routes $ \route ->
    checkRouteEncoderGivenRoute enc model route
      `whenLeft_` throwError
  -- For Github Pages
  noBirdbrainedJekyll dest
  -- Enumerate and write all routes.
  log LevelInfo $ "Writing " <> show (length routes) <> " routes"
  fmap concat . forM routes $ \r -> do
    let fp = dest </> review rp r
    case siteOutput rp model r of
      AssetStatic staticPath -> do
        liftIO (doesPathExist staticPath) >>= \case
          True ->
            -- NOTE: A static path can indeed be a directory. The user is not
            -- obliged to recursively list the files.
            copyRecursively staticPath fp
          False ->
            log LevelError $ toText $ "? " <> staticPath <> " (missing)"
        pure []
      AssetGenerated _fmt !s -> do
        writeFileGenerated fp s
        pure [fp]

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

writeFileGenerated :: (MonadLogger m, MonadIO m) => FilePath -> LByteString -> m ()
writeFileGenerated fp s = do
  log LevelInfo $ toText $ "W " <> fp
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory fp)
    writeFileLBS fp s

{- | Copy a file or directory recursively to the target directory

  Like `cp -R src dest`.
-}
copyRecursively ::
  forall m.
  ( MonadIO m
  , MonadLoggerIO m
  ) =>
  -- | Absolute path to source file or directory to copy.
  FilePath ->
  -- | Target file or directory path.
  FilePath ->
  m ()
copyRecursively src dest = do
  fs <- enumerateFilesToCopy src dest
  forM_ fs $ \(a, b) -> do
    log LevelInfo $ toText $ "C " <> b
    copyFileCreatingParents a b
  where
    enumerateFilesToCopy :: FilePath -> FilePath -> m [(FilePath, FilePath)]
    enumerateFilesToCopy a b = do
      liftIO (doesFileExist a) >>= \case
        True ->
          pure [(a, b)]
        False -> do
          liftIO (doesDirectoryExist a) >>= \case
            False ->
              liftIO $ throwIO $ StaticAssetMissing a
            True -> do
              fs <- liftIO $ getDirectoryFiles src ["**"]
              pure $ fs <&> \fp -> (a </> fp, b </> fp)

    copyFileCreatingParents a b =
      liftIO $ do
        createDirectoryIfMissing True (takeDirectory b)
        copyFile a b
