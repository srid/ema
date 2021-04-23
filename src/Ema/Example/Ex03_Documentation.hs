{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | An advanced example demonstrating how to build documentation sites.
--
-- This "example" is actually used to build Ema's documentation site itself.
module Ema.Example.Ex03_Documentation where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import Data.Tagged
import Ema (Ema (..), Slug (unSlug), routeUrl, runEma)
import qualified Ema.Helper.Tailwind as Tailwind
import System.FSNotify (Event (..), watchDir, withManager)
import System.FilePath (splitExtension, splitPath, (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type SourcePath = Tagged "SourcePath" (NonEmpty Text)

instance One SourcePath where
  type OneItem SourcePath = Text
  one x = Tagged $ x :| []

mkSourcePath :: FilePath -> Maybe SourcePath
mkSourcePath = \case
  (splitExtension -> (fp, ".md")) ->
    Tagged . fmap toText <$> nonEmpty (splitPath fp)
  _ ->
    Nothing

data Route
  = Page SourcePath
  deriving (Show)

newtype Sources = Sources {unSources :: Map SourcePath Text}
  deriving (Show)

instance Ema Sources Route where
  encodeRoute = \case
    Page (Tagged ("index" :| [])) -> mempty
    Page (Tagged paths) -> toList . fmap (fromString . toString) $ paths
  decodeRoute = \case
    [] -> Just (Page $ one "index")
    (slug : rest) ->
      Just $ Page $ Tagged $ fmap (toText . unSlug) $ slug :| rest
  staticRoutes (Map.keys . unSources -> spaths) =
    Page <$> spaths

main :: IO ()
main = do
  mainWith "docs"

mainWith :: FilePath -> IO ()
mainWith folder = do
  runEma render $ \model -> do
    LVar.set model =<< loadSources
    watchAndUpdate model
  where
    loadSources :: IO Sources
    loadSources = do
      putStrLn $ "Loading .md files from " <> folder
      fs <- getDirectoryFiles folder (one "*.md")
      Sources . Map.fromList . catMaybes <$> forM fs (readSource . (folder </>))

    -- Watch the diary folder, and update our in-memory model incrementally.
    watchAndUpdate :: LVar.LVar Sources -> IO ()
    watchAndUpdate model = do
      putStrLn $ "Watching .org files in " <> folder
      withManager $ \mgr -> do
        stop <- watchDir mgr folder (const True) $ \event -> do
          print event
          let updateFile fp = do
                readSource fp >>= \case
                  Nothing -> pure ()
                  Just (spath, s) -> do
                    putStrLn $ "Update: " <> show spath
                    LVar.modify model $ Sources . Map.insert spath s . unSources
              deleteFile fp = do
                whenJust (mkSourcePath fp) $ \spath -> do
                  putStrLn $ "Delete: " <> show spath
                  LVar.modify model $ Sources . Map.delete spath . unSources
          case event of
            Added fp _ isDir -> unless isDir $ updateFile fp
            Modified fp _ isDir -> unless isDir $ updateFile fp
            Removed fp _ isDir -> unless isDir $ deleteFile fp
            Unknown fp _ _ -> updateFile fp
        threadDelay maxBound
          `finally` stop

readSource :: FilePath -> IO (Maybe (SourcePath, Text))
readSource fp =
  runMaybeT $ do
    spath :: SourcePath <- MaybeT $ pure $ mkSourcePath fp
    s <- readFileText fp
    pure (spath, s)

render :: Sources -> Route -> LByteString
render diary (Page spath) = do
  Tailwind.layout (H.title "Ema Docs") $
    H.div ! A.class_ "container mx-auto" $ do
      H.pre $ show spath
      H.footer ! A.class_ "mt-2 text-center border-t-2 text-gray-500" $ do
        "Powered by "
        H.a ! A.href "https://github.com/srid/ema" ! A.target "blank_" $ "Ema"
  where
    routeElem r' w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ routeUrl r')
