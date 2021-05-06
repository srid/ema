{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Helper to read a directory of files, and observe it for changes.
--
-- Use @new@ in conjunction with @observe@ in your @runEma@ function call.
module Ema.Helper.Markdown
  ( -- Parsing
    parseMarkdownWithFrontMatter,
    parseMarkdown,
    -- Utilities
    plainify,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Pandoc as CP
import Control.Monad.Combinators (manyTill)
import qualified Data.YAML as Y
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W

newtype BadMarkdown = BadMarkdown Text
  deriving (Show, Exception)

-- | Parse a Markdown file using commonmark-hs with all extensions enabled
parseMarkdownWithFrontMatter ::
  forall meta.
  Y.FromYAML meta =>
  -- | Path to file associated with this Markdown
  FilePath ->
  -- | Markdown text to parse
  Text ->
  Either Text (Maybe meta, Pandoc)
parseMarkdownWithFrontMatter fn s = do
  (mMeta, markdown) <- partitionMarkdown fn s
  mMetaVal <- parseYaml fn `traverse` mMeta
  blocks <- first show $ join $ CM.commonmarkWith @(Either CM.ParseError) fullMarkdownSpec fn markdown
  let doc = Pandoc mempty $ B.toList . CP.unCm @() @B.Blocks $ blocks
  pure (mMetaVal, doc)

-- | Like `parseMarkdownWithFrontMatter` but assumes that no YAML frontmatter is present.
parseMarkdown :: FilePath -> Text -> Either Text Pandoc
parseMarkdown fn s = do
  cmBlocks <- first show $ join $ CM.commonmarkWith @(Either CM.ParseError) fullMarkdownSpec fn s
  let blocks = B.toList . CP.unCm @() @B.Blocks $ cmBlocks
  pure $ Pandoc mempty blocks

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

fullMarkdownSpec ::
  SyntaxSpec' m il bl =>
  CM.SyntaxSpec m il bl
fullMarkdownSpec =
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
      -- fancyListSpec and defaultSyntaxSpec to avoid bad results when parsing
      -- non-table lines
      CE.pipeTableSpec
    ]

-- | Identify metadata block at the top, and split it from markdown body.
--
-- FIXME: https://github.com/srid/neuron/issues/175
partitionMarkdown :: FilePath -> Text -> Either Text (Maybe Text, Text)
partitionMarkdown =
  parse (M.try splitP <|> fmap (Nothing,) M.takeRest)
  where
    separatorP :: M.Parsec Void Text ()
    separatorP =
      void $ M.string "---" <* M.eol
    splitP :: M.Parsec Void Text (Maybe Text, Text)
    splitP = do
      separatorP
      a <- toText <$> manyTill M.anySingle (M.try $ M.eol *> separatorP)
      b <- M.takeRest
      pure (Just a, b)
    parse :: M.Parsec Void Text a -> String -> Text -> Either Text a
    parse p fn s =
      first (toText . M.errorBundlePretty) $
        M.parse (p <* M.eof) fn s

-- NOTE: HsYAML parsing is rather slow due to its use of DList.
-- See https://github.com/haskell-hvr/HsYAML/issues/40
parseYaml :: Y.FromYAML a => FilePath -> Text -> Either Text a
parseYaml n (encodeUtf8 -> v) = do
  let mkError (loc, emsg) =
        toText $ n <> ":" <> Y.prettyPosWithSource loc v " error" <> emsg
  first mkError $ Y.decode1 v

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
