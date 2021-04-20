{-# LANGUAGE TypeApplications #-}

-- | An advanced example demonstrating how to build something like neuron
--
-- Create a nice a looking website with calendar view and outlines out of your
-- daily notes written in org-mode format.
module Ema.Example.Ex03_Diary where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import qualified Data.Map.Strict as Map
import Data.Org (OrgFile)
import qualified Data.Org as Org
import qualified Data.Text as T
import Data.Time
import Ema.App (Ema (..), runEma)
import qualified Ema.Changing as Changing
import qualified Ema.Layout as Layout
import Ema.Route
import qualified Shower
import System.FSNotify
import System.FilePath
import System.FilePattern.Directory
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
    OnDay day -> ["day", show day]
  fromSlug = \case
    [] -> Just Index
    ["day", s] -> OnDay <$> parseDay (toString $ unSlug s)
    _ -> Nothing

parseDay :: String -> Maybe Day
parseDay =
  parseTimeM False defaultTimeLocale "%Y-%m-%d"

type Diary = Map Day OrgFile

watchDairyFolder :: FilePath -> Changing.Changing Diary -> IO ()
watchDairyFolder folder model = do
  fs <- getDirectoryFiles folder (one "*.org")
  print fs
  model0 <- fmap (Map.fromList . catMaybes) $
    forM fs $ \f -> do
      case parseDay . toString =<< T.stripSuffix ".org" (toText f) of
        Nothing -> pure Nothing
        Just day -> do
          putStrLn f
          s <- readFileText $ folder </> f
          pure $ (day,) <$> Org.org s
  Changing.set model model0
  -- TODO: watch
  threadDelay maxBound

main :: IO ()
main = do
  model <- Changing.new @Diary mempty
  race_
    (runEma $ Ema model render)
    (watchDairyFolder "/home/srid/KB" model)
  where
    render (diary :: Diary) (r :: Route) =
      Layout.tailwindSite (H.title "Diary") $
        H.div ! A.class_ "container mx-auto" $ do
          case r of
            Index -> do
              H.header "Daily notes"
              H.div ! A.class_ "" $
                forM_ (Map.keys diary) $ \day ->
                  routeElem (OnDay day) $ "Day " >> H.toMarkup @Text (show day)
            OnDay day -> do
              routeElem Index "Back to Index"
              case Map.lookup day diary of
                Nothing -> "not found"
                Just org -> do
                  renderOrg org
                  H.div ! A.class_ "border-t-2 mt-8 bg-gray-100 text-xs" $ do
                    H.pre $ H.toMarkup (Shower.shower org)
    routeElem r w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r $ w
    routeHref r =
      A.href (fromString . toString $ routeUrl r)

renderOrg :: OrgFile -> H.Html
renderOrg (Org.OrgFile meta doc) = do
  let heading = H.header ! A.class_ "text-2xl my-2 font-bold"
  heading "Meta"
  H.table ! A.class_ "Metatable-auto" $ do
    let td cls = H.td ! A.class_ ("border px-4 py-2 " <> cls)
    forM_ (Map.toList meta) $ \(k, v) ->
      H.tr $ do
        td "font-bold" $ H.toMarkup k
        td "font-mono" $ H.toMarkup v
  heading "Doc"
  renderOrgDoc doc

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
      H.toMarkup (Org.prettyWords s) >> " "
    forM_ tags $ \tag ->
      H.span ! A.class_ "border-1 p-2 bg-gray-200 rounded" $ H.toMarkup tag
    renderOrgDoc doc
