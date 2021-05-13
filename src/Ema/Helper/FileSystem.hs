{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Helper to read a directory of files, and observe it for changes.
--
-- TODO: Publish this to hackage as an addon library for `lvar` (after renaming
-- lvar package)?
module Ema.Helper.FileSystem
  ( -- | This is typically what you want.
    mountOnLVar,
    -- | Lower-level utilities
    filesMatching,
    onChange,
    FileAction (..),
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad.Logger
  ( LogLevel (LevelDebug, LevelInfo),
    MonadLogger,
    logWithoutLoc,
  )
import Data.Default (Default (..))
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import System.Directory (canonicalizePath)
import System.FSNotify
  ( ActionPredicate,
    Event (..),
    StopListening,
    WatchManager,
    watchTree,
    withManager,
  )
import System.FilePath (isRelative, makeRelative)
import System.FilePattern (FilePattern, matchMany)
import System.FilePattern.Directory (getDirectoryFiles)
import UnliftIO (MonadUnliftIO, withRunInIO)

-- | Mount the given directory on to the given LVar such that any filesystem
-- events (represented by `FileAction`) are made to be reflected in the LVar
-- model using the given model update function.
mountOnLVar ::
  forall model m b.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLogger m,
    Default model,
    Show b
  ) =>
  -- | The directory to mount.
  FilePath ->
  -- | Only include these files (exclude everything else)
  [(b, FilePattern)] ->
  -- | The `LVar` onto which to mount.
  LVar model ->
  -- | How to update the model given a file action.
  ((b, FilePath) -> FileAction -> m (model -> model)) ->
  m ()
mountOnLVar folder pats var toAction = do
  log LevelInfo $ "Mounting path " <> toText folder <> " (filter: " <> show pats <> ") onto LVar"
  LVar.set var =<< do
    fs <-
      filesMatching folder (snd <$> pats) <&> \xs ->
        mapMaybe (\x -> (,x) <$> getTag x) xs
    initialActions <- traverse (`toAction` Update) fs
    pure $ foldl' (flip ($)) def initialActions
  onChange folder $ \fp change -> do
    whenJust (getTag fp) $ \tag ->
      LVar.modify var =<< toAction (tag, fp) change
  where
    getTag :: FilePath -> Maybe b
    getTag fp =
      let pull patterns =
            fmap (\(x, _, _) -> x) $ listToMaybe $ matchMany patterns (one ((), fp))
       in if isRelative fp
            then pull pats
            else -- `fp` is an absolute path (because of use of symlinks), so let's
            -- be more lenient in matching it. Note that this does meat we might
            -- match files the user may not have originally intended. This is
            -- the trade offs with using symlinks.
              pull $ second ("**/" <>) <$> pats

filesMatching :: (MonadIO m, MonadLogger m) => FilePath -> [FilePattern] -> m [FilePath]
filesMatching parent' pats = do
  parent <- liftIO $ canonicalizePath parent'
  log LevelInfo $ toText $ "Traversing " <> parent <> " for files matching " <> show pats
  liftIO $ getDirectoryFiles parent pats

data FileAction = Update | Delete
  deriving (Eq, Show)

onChange ::
  forall m.
  (MonadIO m, MonadLogger m, MonadUnliftIO m) =>
  FilePath ->
  -- | The filepath is relative to the folder being monitored, unless if its
  -- ancestor is a symlink.
  (FilePath -> FileAction -> m ()) ->
  m ()
onChange parent' f = do
  -- NOTE: It is important to use canonical path, because this will allow us to
  -- transform fsnotify event's (absolute) path into one that is relative to
  -- @parent'@ (as passed by user), which is what @f@ will expect.
  parent <- liftIO $ canonicalizePath parent'
  withManagerM $ \mgr -> do
    log LevelInfo $ toText $ "Monitoring " <> parent <> " for changes"
    stop <- watchTreeM mgr parent (const True) $ \event -> do
      log LevelDebug $ show event
      let rel = makeRelative parent
      case event of
        Added (rel -> fp) _ _ -> f fp Update
        Modified (rel -> fp) _ _ -> f fp Update
        Removed (rel -> fp) _ _ -> f fp Delete
        Unknown (rel -> fp) _ _ -> f fp Delete
    liftIO $ threadDelay maxBound `finally` stop

withManagerM ::
  (MonadIO m, MonadUnliftIO m) =>
  (WatchManager -> m a) ->
  m a
withManagerM f = do
  withRunInIO $ \run ->
    withManager $ \mgr -> run (f mgr)

watchTreeM ::
  forall m.
  (MonadIO m, MonadUnliftIO m) =>
  WatchManager ->
  FilePath ->
  ActionPredicate ->
  (Event -> m ()) ->
  m StopListening
watchTreeM wm fp pr f =
  withRunInIO $ \run ->
    watchTree wm fp pr $ \evt -> run (f evt)

log :: MonadLogger m => LogLevel -> Text -> m ()
log = logWithoutLoc "Helper.FileSystem"
