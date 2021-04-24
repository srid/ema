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
import qualified Text.Pandoc.Walk as W

-- | Represents the relative path to a source (.md) file under some directory.
type SourcePath = Tagged "SourcePath" (NonEmpty Text)

indexSourcePath :: SourcePath
indexSourcePath = Tagged $ "index" :| []

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
      case Map.lookup spath (untag srcs) of
        Nothing -> throw $ BadRoute spath
        Just doc -> do
          renderPandoc $
            doc
              & rewriteLinks (\url -> maybe url routeUrl $ mkSourcePath $ toString url)
          -- Debug
          H.div ! A.class_ "text-xs border-2 p-2 bg-gray-100" $ H.pre $ H.toHtml $ Shower.shower doc
          H.footer ! A.class_ "mt-2 text-center border-t-2 text-gray-500" $ do
            "Powered by "
            H.a ! A.href "https://github.com/srid/ema" ! A.target "blank_" $ "Ema"
  where
    routeElem r' w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ routeUrl r')

-- Pandoc transformer

rewriteLinks :: (Text -> Text) -> Pandoc -> Pandoc
rewriteLinks f =
  W.walk $ \case
    B.Link attr is (url, title) ->
      B.Link attr is (f url, title)
    x -> x

-- H.a ! A.href (H.textValue url) ! A.title (H.textValue title) ! rpAttr attr $ mapM_ rpInline is

-- Pandoc renderer

renderPandoc :: Pandoc -> H.Html
renderPandoc (Pandoc _meta blocks) = do
  mapM_ rpBlock blocks

rpBlock :: B.Block -> H.Html
rpBlock = \case
  B.Plain is ->
    mapM_ rpInline is
  B.Para is ->
    H.p ! A.class_ "my-2" $ mapM_ rpInline is
  B.LineBlock iss ->
    forM_ iss $ \is ->
      mapM_ rpInline is >> "\n"
  B.CodeBlock attr s ->
    H.code ! rpAttr attr $ H.pre $ H.text s
  B.RawBlock _ _ ->
    throw Unsupported
  B.BlockQuote bs ->
    H.blockquote $ mapM_ rpBlock bs
  B.OrderedList _ bss ->
    H.ol $ forM_ bss $ \bs -> H.li $ mapM_ rpBlock bs
  B.BulletList bss ->
    H.ul ! A.class_ "list-disc" $ forM_ bss $ \bs -> H.li $ mapM_ rpBlock bs
  B.DefinitionList defs ->
    H.dl $
      forM_ defs $ \(term, descList) -> do
        mapM_ rpInline term
        forM_ descList $ \desc ->
          H.dd $ mapM_ rpBlock desc
  B.Header level attr is ->
    headerElem level ! rpAttr attr $ mapM_ rpInline is
  B.HorizontalRule ->
    H.hr
  B.Table {} ->
    throw Unsupported
  B.Div attr bs ->
    H.div ! rpAttr attr $ mapM_ rpBlock bs
  B.Null ->
    pure ()

headerElem :: Int -> H.Html -> H.Html
headerElem = \case
  1 -> H.h1 ! A.class_ ("text-6xl " <> my <> " border-b-2 py-2")
  2 -> H.h2 ! A.class_ ("text-5xl " <> my)
  3 -> H.h3 ! A.class_ ("text-4xl " <> my)
  4 -> H.h4 ! A.class_ ("text-3xl " <> my)
  5 -> H.h5 ! A.class_ ("text-2xl " <> my)
  6 -> H.h6 ! A.class_ ("text-xl " <> my)
  _ -> error "Invalid pandoc header level"
  where
    my = "my-2"

rpInline :: B.Inline -> H.Html
rpInline = \case
  B.Str s -> H.toHtml s
  B.Emph is ->
    H.em $ mapM_ rpInline is
  B.Strong is ->
    H.strong $ mapM_ rpInline is
  B.Underline is ->
    H.u $ mapM_ rpInline is
  B.Strikeout is ->
    -- FIXME: Should use <s>, but blaze doesn't have it.
    H.del $ mapM_ rpInline is
  B.Superscript is ->
    H.sup $ mapM_ rpInline is
  B.Subscript is ->
    H.sub $ mapM_ rpInline is
  B.Quoted qt is ->
    flip inQuotes qt $ mapM_ rpInline is
  B.Code attr s ->
    H.code ! rpAttr attr $ H.toHtml s
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> H.br
  B.RawInline _fmt s ->
    H.pre $ H.toHtml s
  B.Math _ _ ->
    throw Unsupported
  B.Link attr is (url, title) ->
    H.a ! A.href (H.textValue url) ! A.title (H.textValue title) ! rpAttr attr $ mapM_ rpInline is
  B.Image attr is (url, title) ->
    H.img ! A.src (H.textValue url) ! A.title (H.textValue title) ! A.alt (H.textValue $ plainify is) ! rpAttr attr
  B.Note _ ->
    throw Unsupported
  B.Span attr is ->
    H.span ! rpAttr attr $ mapM_ rpInline is
  x ->
    H.pre $ H.toHtml $ show @Text x
  where
    inQuotes :: H.Html -> B.QuoteType -> H.Html
    inQuotes w = \case
      B.SingleQuote -> "‘" >> w <* "’"
      B.DoubleQuote -> "“" >> w <* "”"

rpAttr :: B.Attr -> H.Attribute
rpAttr (id', classes, attrs) =
  A.id (fromString . toString $ id')
    <> A.class_ (fromString . toString $ T.intercalate " " classes)
    <> mconcat (fmap (\(k, v) -> H.dataAttribute (fromString . toString $ k) (fromString . toString $ v)) attrs)

data Unsupported = Unsupported
  deriving (Show, Exception)

-- Pandoc helpers

getPandocH1 :: Pandoc -> Maybe [B.Inline]
getPandocH1 = listToMaybe . W.query go
  where
    go :: B.Block -> [[B.Inline]]
    go = \case
      B.Header 1 _ inlines ->
        [inlines]
      _ ->
        []

-- | Convert Pandoc AST inlines to raw text.
plainify :: [B.Inline] -> Text
plainify = W.query $ \case
  B.Str x -> x
  B.Code _attr x -> x
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> " "
  B.RawInline _fmt s -> s
  B.Math _mathTyp s -> s
  -- Ignore the rest of AST nodes, as they are recursively defined in terms of
  -- `Inline` which `W.query` will traverse again.
  _ -> ""

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
