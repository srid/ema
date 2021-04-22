{-# LANGUAGE TypeApplications #-}

-- | An advanced example demonstrating how to build something like neuron
--
-- Create a nice a looking website with calendar view and outlines out of your
-- daily notes written in org-mode format.
module Ema.Example.Ex03_Diary where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import Data.Org (OrgFile)
import qualified Data.Org as Org
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Ema.App (runEma)
import qualified Ema.Helper.Tailwind as Tailwind
import Ema.Route
import qualified Shower
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import System.FSNotify
  ( Event (..),
    watchDir,
    withManager,
  )
import System.FilePath (takeFileName, (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = Index
  | OnDay Day
  deriving (Show)

instance IsRoute Route where
  toSlug = \case
    Index -> mempty
    OnDay day -> one $ show day
  fromSlug = \case
    [] -> Just Index
    [s] -> OnDay <$> parseDay (toString $ unSlug s)
    _ -> Nothing

parseDay :: String -> Maybe Day
parseDay =
  parseTimeM False defaultTimeLocale "%Y-%m-%d"

parseDailyNote :: FilePath -> IO (Maybe (Day, OrgFile))
parseDailyNote f =
  case parseDailyNoteFilepath f of
    Nothing -> pure Nothing
    Just day -> do
      s <- readFileText f
      pure $ (day,) <$> Org.org s

parseDailyNoteFilepath :: FilePath -> Maybe Day
parseDailyNoteFilepath f =
  parseDay . toString =<< T.stripSuffix ".org" (toText $ takeFileName f)

type Diary = Map Day OrgFile

diaryFrom :: FilePath -> IO Diary
diaryFrom folder = do
  putStrLn $ "Loading .org files from " <> folder
  fs <- getDirectoryFiles folder (one "*.org")
  Map.fromList . catMaybes <$> forM fs (parseDailyNote . (folder </>))

watchAndUpdateDiary :: FilePath -> LVar.LVar Diary -> IO ()
watchAndUpdateDiary folder model = do
  putStrLn $ "Watching .org files in " <> folder
  withManager $ \mgr -> do
    stop <- watchDir mgr folder (const True) $ \event -> do
      print event
      let updateFile fp = do
            parseDailyNote fp >>= \case
              Nothing -> pure ()
              Just (day, org) -> do
                putStrLn $ "Update: " <> show day
                LVar.modify model $ Map.insert day org
          deleteFile fp = do
            whenJust (parseDailyNoteFilepath fp) $ \day -> do
              putStrLn $ "Delete: " <> show day
              LVar.modify model $ Map.delete day
      case event of
        Added fp _ isDir -> unless isDir $ updateFile fp
        Modified fp _ isDir -> unless isDir $ updateFile fp
        Removed fp _ isDir -> unless isDir $ deleteFile fp
        Unknown fp _ _ -> updateFile fp
    threadDelay maxBound
      `finally` stop

main :: IO ()
main = do
  folder <-
    getArgs >>= \case
      [_, path] -> canonicalizePath path
      _ -> canonicalizePath "src/Ema/Example/Diary"
  mainWith folder

mainWith :: FilePath -> IO ()
mainWith folder = do
  flip runEma render $ \model -> do
    LVar.set model =<< diaryFrom folder
    watchAndUpdateDiary folder model

render :: Diary -> Route -> LByteString
render diary r = do
  Tailwind.layout (H.title pageTitle) $
    H.div ! A.class_ "container mx-auto" $ do
      let heading =
            H.header
              ! A.class_ "text-4xl my-2 py-2 font-bold text-center bg-purple-50 shadow"
      case r of
        Index -> do
          heading "My Diary"
          H.div ! A.class_ "" $
            forM_ (sortOn Down $ Map.keys diary) $ \day ->
              H.li $ routeElem (OnDay day) $ H.toMarkup @Text (show day)
        OnDay day -> do
          heading $ show day
          routeElem Index "Back to Index"
          maybe "not found" renderOrg (Map.lookup day diary)
      H.footer ! A.class_ "mt-2 text-center border-t-2 text-gray-500" $ do
        "Powered by "
        H.a ! A.href "https://github.com/srid/ema" ! A.target "blank_" $ "Ema"
  where
    pageTitle = case r of
      Index -> "My Diary"
      OnDay day -> show day <> " -- My Diary"
    routeElem r' w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ routeUrl r')

renderOrg :: OrgFile -> H.Html
renderOrg _org@(Org.OrgFile meta doc) = do
  let heading = H.header ! A.class_ "text-2xl my-2 font-bold"
  unless (null meta) $ do
    heading "Meta"
    renderMeta meta
  heading "Doc"
  -- Debug dump
  -- H.pre $ H.toMarkup (Shower.shower org)
  renderOrgDoc doc

renderMeta :: Map Text Text -> H.Html
renderMeta meta = do
  H.table ! A.class_ "table-auto" $ do
    let td cls = H.td ! A.class_ ("border px-4 py-2 " <> cls)
    forM_ (Map.toList meta) $ \(k, v) ->
      H.tr $ do
        td "font-bold" $ H.toMarkup k
        td "font-mono" $ H.toMarkup v

renderOrgDoc :: Org.OrgDoc -> H.Html
renderOrgDoc (Org.OrgDoc blocks sections) = do
  H.ul ! A.class_ "list-disc ml-8" $ do
    whenNotNull blocks $ \_ -> do
      H.header ! A.class_ "text-2xl font-bold" $ "Blocks"
      H.pre $ H.toMarkup (Shower.shower blocks)
    whenNotNull sections $ \_ -> do
      forM_ sections renderSection

renderSection :: Org.Section -> H.Html
renderSection (Org.Section heading tags doc) = do
  H.li $ do
    forM_ heading $ \s ->
      renderWords s >> " "
    forM_ tags renderTag
    renderOrgDoc doc

renderTag :: Text -> H.Html
renderTag tag =
  H.span
    ! A.class_ "border-1 p-0.5 bg-purple-200 font-bold rounded"
    ! A.title "Tag"
    $ H.toMarkup tag

renderWords :: Org.Words -> H.Markup
renderWords ws = do
  let s = Org.prettyWords ws
  if s `Set.member` Set.fromList ["TODO", "DONE"]
    then
      H.span
        ! A.class_ "border-1 p-0.5 bg-gray-600 text-white"
        ! A.title "Keyword"
        $ H.toMarkup s
    else H.toMarkup s
