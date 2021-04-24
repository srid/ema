{-# LANGUAGE TypeFamilies #-}

-- | Helper to read a directory of files, and observe it for changes.
--
-- Use @new@ in conjunction with @observe@ in your @runEma@ function call.
module Ema.Helper.FileSystem where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import System.Directory (canonicalizePath)
import System.FSNotify
  ( Event (..),
    watchTree,
    withManager,
  )
import System.FilePath (makeRelative)
import System.FilePattern (FilePattern)
import System.FilePattern.Directory (getDirectoryFiles)

type FolderPath = FilePath

filesMatching :: FolderPath -> [FilePattern] -> IO [FilePath]
filesMatching parent' pats = do
  parent <- canonicalizePath parent'
  getDirectoryFiles parent pats

data FileAction = Update | Delete
  deriving (Eq, Show)

onChange :: FolderPath -> (FilePath -> FileAction -> IO ()) -> IO ()
onChange parent' f = do
  -- NOTE: It is important to use canonical path, because this will allow us to
  -- transform fsnotify event's (absolute) path into one that is relative to
  -- @parent'@ (as passed by user), which is what @f@ will expect.
  parent <- canonicalizePath parent'
  withManager $ \mgr -> do
    stop <- watchTree mgr parent (const True) $ \event -> do
      print event
      let rel = makeRelative parent
      case event of
        Added (rel -> fp) _ _ -> f fp Update
        Modified (rel -> fp) _ _ -> f fp Update
        Removed (rel -> fp) _ _ -> f fp Delete
        Unknown (rel -> fp) _ _ -> f fp Delete
    threadDelay maxBound
      `finally` stop
