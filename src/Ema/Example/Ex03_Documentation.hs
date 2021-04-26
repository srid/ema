{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | An advanced example demonstrating how to build documentation sites.
--
-- This "example" is actually used to build Ema's documentation site itself. It
-- is a work in progress currently.
module Ema.Example.Ex03_Documentation where

import qualified Commonmark as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Pandoc as CP
import Control.Exception (throw)
import Control.Monad.Logger
import qualified Data.LVar as LVar
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Profunctor (dimap)
import Data.Tagged (Tagged (Tagged), untag)
import qualified Data.Text as T
import Ema (Ema (..), Slug (unSlug), routeUrl, runEma)
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Tailwind as Tailwind
import NeatInterpolation (text)
import System.FilePath (splitExtension, splitPath)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W

-- | Represents the relative path to a source (.md) file under some directory.
type MarkdownPath = Tagged "MarkdownPath" (NonEmpty Text)

indexMarkdownPath :: MarkdownPath
indexMarkdownPath = Tagged $ "index" :| []

mkMarkdownPath :: FilePath -> Maybe MarkdownPath
mkMarkdownPath = \case
  (splitExtension -> (fp, ".md")) ->
    let slugs = T.dropWhileEnd (== '/') . toText <$> splitPath fp
     in Tagged <$> nonEmpty slugs
  _ ->
    Nothing

markdownPathFileBase :: MarkdownPath -> Text
markdownPathFileBase (Tagged slugs) =
  head $ NE.reverse slugs

markdownPathInits :: MarkdownPath -> NonEmpty MarkdownPath
markdownPathInits (Tagged ("index" :| [])) =
  one indexMarkdownPath
markdownPathInits (Tagged (slug :| rest')) =
  indexMarkdownPath :| case nonEmpty rest' of
    Nothing ->
      one $ Tagged (one slug)
    Just rest ->
      Tagged (one slug) : go (one slug) rest
  where
    go :: NonEmpty Text -> NonEmpty Text -> [MarkdownPath]
    go x (y :| ys') =
      let this = Tagged (x <> one y)
       in case nonEmpty ys' of
            Nothing ->
              one this
            Just ys ->
              this : go (untag this) ys

type MarkdownSources = Tagged "MarkdownSources" (Map MarkdownPath Pandoc)

instance Ema MarkdownSources MarkdownPath where
  encodeRoute = \case
    Tagged ("index" :| []) -> mempty
    Tagged paths -> toList . fmap (fromString . toString) $ paths
  decodeRoute = \case
    (nonEmpty -> Nothing) ->
      pure $ Tagged $ one "index"
    (nonEmpty -> Just slugs) -> do
      let parts = toText . unSlug <$> slugs
      -- Heuristic to let requests to static files (eg: favicon.ico) to pass through
      guard $ not (any (T.isInfixOf ".") parts)
      pure $ Tagged parts
  staticRoutes (Map.keys . untag -> spaths) =
    spaths
  staticAssets _ =
    ["manifest.json", "ema.svg"]

main :: IO ()
main =
  runEma render $ \model -> do
    LVar.set model =<< do
      mdFiles <- FileSystem.filesMatching "." ["**/*.md"]
      liftIO $
        forM mdFiles readSource
          <&> Tagged . Map.fromList . catMaybes
    FileSystem.onChange "." $ \fp -> \case
      FileSystem.Update ->
        whenJustM (readSource fp) $ \(spath, s) -> do
          logInfoN $ "Update: " <> show spath
          LVar.modify model $ Tagged . Map.insert spath s . untag
      FileSystem.Delete ->
        whenJust (mkMarkdownPath fp) $ \spath -> do
          logInfoN $ "Delete: " <> show spath
          LVar.modify model $ Tagged . Map.delete spath . untag
  where
    readSource :: MonadIO m => FilePath -> m (Maybe (MarkdownPath, Pandoc))
    readSource fp =
      runMaybeT $ do
        spath :: MarkdownPath <- MaybeT $ pure $ mkMarkdownPath fp
        s <- readFileText fp
        pure (spath, parseMarkdown s)

newtype BadRoute = BadRoute MarkdownPath
  deriving (Show, Exception)

render :: Ema.CLI.Action -> MarkdownSources -> MarkdownPath -> LByteString
render emaAction srcs spath = do
  case Map.lookup spath (untag srcs) of
    Nothing -> throw $ BadRoute spath
    Just doc -> do
      Tailwind.layout emaAction (headHtml spath doc) (bodyHtml srcs spath doc)

headHtml :: MarkdownPath -> Pandoc -> H.Html
headHtml spath doc = do
  let siteTitle = "Ema"
      routeTitle = maybe (last $ untag spath) plainify $ getPandocH1 doc
  H.title $
    H.text $
      if routeTitle == siteTitle then siteTitle else routeTitle <> " – " <> siteTitle
  H.meta ! A.name "description" ! A.content "Ema static site generator (Jamstack) in Haskell"
  favIcon
  -- Make this a PWA and w/ https://web.dev/themed-omnibox/
  H.link ! A.rel "manifest" ! A.href "/manifest.json"
  H.meta ! A.name "theme-color" ! A.content "#d53f8c"
  unless (spath == indexMarkdownPath) prismJs
  where
    prismJs = do
      H.unsafeByteString . encodeUtf8 $
        [text|
        <link href="https://cdn.jsdelivr.net/npm/prismjs@1.23.0/themes/prism-tomorrow.css" rel="stylesheet" />
        <script src="https://cdn.jsdelivr.net/combine/npm/prismjs@1.23.0/prism.min.js,npm/prismjs@1.23.0/plugins/autoloader/prism-autoloader.min.js"></script>
        |]
    favIcon = do
      H.unsafeByteString . encodeUtf8 $
        [text|
        <link href="/ema.svg" rel="icon" />
        |]

bodyHtml :: MarkdownSources -> MarkdownPath -> Pandoc -> H.Html
bodyHtml srcs spath doc = do
  H.div ! A.class_ "flex justify-center p-4 bg-red-500 text-gray-100 font-bold text-2xl" $ do
    H.div $ do
      H.b "WIP: "
      "Documentation is still being written"
  H.div ! A.class_ "container mx-auto xl:max-w-screen-lg" $ do
    H.div ! A.class_ "px-2" $ do
      renderBreadcrumbs srcs spath
      renderPandoc $
        doc
          & applyClassLibrary (\c -> fromMaybe c $ Map.lookup c emaMarkdownStyleLibrary)
          & rewriteLinks
            -- Rewrite .md links to @MarkdownPath@
            ( \url -> fromMaybe url $ do
                guard $ not $ "://" `T.isInfixOf` url
                target <- mkMarkdownPath $ toString url
                -- Check that .md links are not broken
                if Map.member target (untag srcs)
                  then pure $ routeUrl target
                  else throw $ BadRoute target
            )
    H.footer ! A.class_ "mt-8 text-center text-gray-500" $ do
      "Powered by "
      H.a ! A.class_ "font-bold" ! A.target "blank" ! A.href "https://github.com/srid/ema" $ "Ema"
  where
    emaMarkdownStyleLibrary =
      Map.fromList
        [ ("feature", "flex justify-center items-center text-center shadow-lg p-2 m-2 w-32 h-16 lg:w-auto rounded border-2 border-gray-400 bg-pink-100 text-base font-bold hover:bg-pink-200 hover:border-black"),
          ("avatar", "float-right w-32 h-32"),
          -- List item specifc styles
          ("item-intro", "text-gray-500"),
          -- Styling the last line in series posts
          ("last", "mt-8 border-t-2 border-pink-500 pb-1 pl-1 bg-gray-50 rounded"),
          ("next", "py-2 text-xl italic font-bold")
        ]

lookupTitleForgiving :: MarkdownSources -> MarkdownPath -> Text
lookupTitleForgiving srcs spath =
  fromMaybe (markdownPathFileBase spath) $ do
    doc <- Map.lookup spath $ untag srcs
    is <- getPandocH1 doc
    pure $ plainify is

renderBreadcrumbs :: MarkdownSources -> MarkdownPath -> H.Html
renderBreadcrumbs srcs spath = do
  whenNotNull (init $ markdownPathInits spath) $ \(toList -> crumbs) ->
    H.div ! A.class_ "w-full text-gray-600 mt-4" $ do
      H.div ! A.class_ "flex justify-center" $ do
        H.div ! A.class_ "w-full bg-white py-2 rounded" $ do
          H.ul ! A.class_ "flex text-gray-500 text-sm lg:text-base" $ do
            forM_ crumbs $ \crumb ->
              H.li ! A.class_ "inline-flex items-center" $ do
                H.a ! A.class_ "px-1 font-bold bg-pink-500 text-gray-50 rounded"
                  ! routeHref crumb
                  $ H.text $ lookupTitleForgiving srcs crumb
                rightArrow
            H.li ! A.class_ "inline-flex items-center text-gray-600" $ do
              H.a $ H.text $ lookupTitleForgiving srcs spath
  where
    rightArrow =
      H.unsafeByteString $
        encodeUtf8
          [text|
          <svg fill="currentColor" viewBox="0 0 20 20" class="h-5 w-auto text-gray-400"><path fill-rule="evenodd" d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z" clip-rule="evenodd"></path></svg>
          |]

routeHref :: Ema a r => r -> H.Attribute
routeHref r' =
  A.href (fromString . toString $ routeUrl r')

-- Pandoc transformer

rewriteLinks :: (Text -> Text) -> Pandoc -> Pandoc
rewriteLinks f =
  W.walk $ \case
    B.Link attr is (url, title) ->
      B.Link attr is (f url, title)
    x -> x

applyClassLibrary :: (Text -> Text) -> Pandoc -> Pandoc
applyClassLibrary f =
  walkBlocks . walkInlines
  where
    walkBlocks = W.walk $ \case
      B.Div attr bs ->
        B.Div (g attr) bs
      x -> x
    walkInlines = W.walk $ \case
      B.Span attr is ->
        B.Span (g attr) is
      x -> x
    g (id', cls, attr) =
      (id', withPackedClass f cls, attr)
    withPackedClass :: (Text -> Text) -> [Text] -> [Text]
    withPackedClass =
      dimap (T.intercalate " ") (T.splitOn " ")

-- Pandoc renderer
--
-- Note that we hardcode tailwind classes, because pandoc AST is not flexible
-- enough to provide attrs for all inlines/blocks. So we can't rely on Walk to
-- transform it.

renderPandoc :: Pandoc -> H.Html
renderPandoc (Pandoc _meta blocks) =
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
  B.CodeBlock (id', classes, attrs) s ->
    -- Prism friendly classes
    let classes' = flip concatMap classes $ \cls -> [cls, "language-" <> cls]
     in H.pre ! rpAttr (id', classes', attrs) $ H.code ! rpAttr ("", classes', []) $ H.text s
  B.RawBlock _ _ ->
    pure ()
  B.BlockQuote bs ->
    H.blockquote $ mapM_ rpBlock bs
  B.OrderedList _ bss ->
    H.ol ! A.class_ listStyle $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
  B.BulletList bss ->
    H.ul ! A.class_ (listStyle <> " list-disc") $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
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
  where
    listStyle = "list-inside ml-2"
    listItemStyle = "text-xl py-1.5 lg:py-0 lg:text-base"

headerElem :: Int -> H.Html -> H.Html
headerElem = \case
  1 -> H.h1 ! A.class_ ("text-6xl " <> my <> " text-center py-2")
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
  B.Link attr is (url, title) -> do
    let (cls, target) =
          if "://" `T.isInfixOf` url
            then ("text-pink-600 hover:underline", A.target "_blank")
            else ("text-pink-600 font-bold hover:bg-pink-50", mempty)
    H.a
      ! A.class_ cls
      ! A.href (H.textValue url)
      ! A.title (H.textValue title)
      ! target
      ! rpAttr attr
      $ mapM_ rpInline is
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
  let cls = T.intercalate " " classes
   in unlessNull id' (A.id (fromString . toString $ id'))
        <> unlessNull cls (A.class_ (fromString . toString $ cls))
        <> mconcat (fmap (\(k, v) -> H.dataAttribute (fromString . toString $ k) (fromString . toString $ v)) attrs)
  where
    unlessNull x f =
      if T.null x then mempty else f

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
