{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | An advanced example demonstrating how to build documentation sites.
--
-- This "example" is actually used to build Ema's documentation site itself.
module Ema.Example.Ex03_Documentation where

import qualified Commonmark as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Pandoc as CP
import Control.Exception (throw)
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import Data.Tagged (Tagged (Tagged), untag)
import qualified Data.Text as T
import Ema (Ema (..), Slug (unSlug), routeUrl, runEma)
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Tailwind as Tailwind
import qualified Shower
import System.FilePath (splitExtension, splitPath, (</>))
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

-- | Represents the relative path to a source (.md) file under some directory.
type SourcePath = Tagged "SourcePath" (NonEmpty Text)

mkSourcePath :: FilePath -> Maybe SourcePath
mkSourcePath = \case
  (splitExtension -> (fp, ".md")) ->
    let slugs = T.dropWhileEnd (== '/') . toText <$> splitPath fp
     in Tagged <$> nonEmpty slugs
  _ ->
    Nothing

type Sources = Tagged "Sources" (Map SourcePath Pandoc)

instance Ema Sources SourcePath where
  encodeRoute = \case
    Tagged ("index" :| []) -> mempty
    Tagged paths -> toList . fmap (fromString . toString) $ paths
  decodeRoute =
    Just . Tagged . \case
      (nonEmpty -> Nothing) ->
        one "index"
      (nonEmpty -> Just slugs) ->
        toText . unSlug <$> slugs
  staticRoutes (Map.keys . untag -> spaths) =
    spaths

main :: IO ()
main = do
  mainWith "docs"

mainWith :: FilePath -> IO ()
mainWith folder = do
  runEma render $ \model -> do
    LVar.set model =<< do
      putStrLn $ "Loading .md files from " <> folder
      mdFiles <- FileSystem.filesMatching folder ["**/*.md"]
      forM mdFiles readSource
        <&> Tagged . Map.fromList . catMaybes
    FileSystem.onChange folder $ \fp -> \case
      FileSystem.Update ->
        whenJustM (readSource fp) $ \(spath, s) -> do
          putStrLn $ "Update: " <> show spath
          LVar.modify model $ Tagged . Map.insert spath s . untag
      FileSystem.Delete ->
        whenJust (mkSourcePath fp) $ \spath -> do
          putStrLn $ "Delete: " <> show spath
          LVar.modify model $ Tagged . Map.delete spath . untag
  where
    readSource :: FilePath -> IO (Maybe (SourcePath, Pandoc))
    readSource fp =
      runMaybeT $ do
        spath :: SourcePath <- MaybeT $ pure $ mkSourcePath fp
        s <- readFileText $ folder </> fp
        pure (spath, parseMarkdown s)

newtype BadRoute = BadRoute SourcePath
  deriving (Show, Exception)

render :: Sources -> SourcePath -> LByteString
render srcs spath = do
  Tailwind.layout (H.title "Ema Docs") $
    H.div ! A.class_ "container mx-auto" $ do
      if spath == Tagged (one "index")
        then H.pre $ H.toHtml $ Shower.shower srcs
        else case Map.lookup spath (untag srcs) of
          Nothing -> throw $ BadRoute spath
          Just doc -> H.pre $ H.toHtml $ Shower.shower doc
      H.footer ! A.class_ "mt-2 text-center border-t-2 text-gray-500" $ do
        "Powered by "
        H.a ! A.href "https://github.com/srid/ema" ! A.target "blank_" $ "Ema"
  where
    routeElem r' w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ routeUrl r')

-- ------------------------
-- Markdown parsing helpers
-- ------------------------

newtype BadMarkdown = BadMarkdown Text
  deriving (Show, Exception)

parseMarkdown :: Text -> Pandoc
parseMarkdown s =
  Pandoc mempty $
    B.toList $
      CP.unCm @() @B.Blocks $
        either (throw . BadMarkdown . show) id $
          join $ CM.commonmarkWith @(Either CM.ParseError) markdownSpec "x" s

type SyntaxSpec' m il bl =
  ( Monad m,
    CM.IsBlock il bl,
    CM.IsInline il,
    Typeable m,
    Typeable il,
    Typeable bl,
    CE.HasEmoji il,
    CE.HasStrikethrough il,
    CE.HasPipeTable il bl,
    CE.HasTaskList il bl,
    CM.ToPlainText il,
    CE.HasFootnote il bl,
    CE.HasMath il,
    CE.HasDefinitionList il bl,
    CE.HasDiv bl,
    CE.HasQuoted il,
    CE.HasSpan il
  )

markdownSpec ::
  SyntaxSpec' m il bl =>
  CM.SyntaxSpec m il bl
markdownSpec =
  mconcat
    [ CE.gfmExtensions,
      CE.fancyListSpec,
      CE.footnoteSpec,
      CE.mathSpec,
      CE.smartPunctuationSpec,
      CE.definitionListSpec,
      CE.attributesSpec,
      CE.rawAttributeSpec,
      CE.fencedDivSpec,
      CE.bracketedSpanSpec,
      CE.autolinkSpec,
      CM.defaultSyntaxSpec,
      -- as the commonmark documentation states, pipeTableSpec should be placed after
      -- fancyListSpec and defaultSyntaxSpec to avoid bad results when non-table lines
      CE.pipeTableSpec
    ]
