{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Helper to deal with Markdown files
--
-- TODO: Publish this eventually to Hackage, along with wiki-link stuff from
-- emanote (maybe as separate package).
module Ema.Helper.Markdown
  ( -- Parsing
    -- TODO: Publish to Hackage as commonmark-pandoc-simple?
    parseMarkdownWithFrontMatter,
    parseMarkdown,
    fullMarkdownSpec,
    fullMarkdownSpecSansPipeTable,
    pipeTableSpec,
    -- Utilities
    plainify,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Pandoc as CP
import Control.Monad.Combinators (manyTill)
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Y
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W

-- | Parse a Markdown file using commonmark-hs with all extensions enabled
parseMarkdownWithFrontMatter ::
  forall meta m il bl.
  ( FromJSON meta,
    m ~ Either CM.ParseError,
    bl ~ CP.Cm () B.Blocks,
    il ~ CP.Cm () B.Inlines
  ) =>
  CM.SyntaxSpec m il bl ->
  -- | Path to file associated with this Markdown
  FilePath ->
  -- | Markdown text to parse
  Text ->
  Either Text (Maybe meta, Pandoc)
parseMarkdownWithFrontMatter spec fn s = do
  (mMeta, markdown) <- partitionMarkdown fn s
  mMetaVal <- first show $ (Y.decodeEither' . encodeUtf8) `traverse` mMeta
  blocks <- first show $ join $ CM.commonmarkWith @(Either CM.ParseError) spec fn markdown
  let doc = Pandoc mempty $ B.toList . CP.unCm @() @B.Blocks $ blocks
  pure (mMetaVal, doc)

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

-- | GFM + official commonmark extensions
fullMarkdownSpec ::
  SyntaxSpec' m il bl =>
  CM.SyntaxSpec m il bl
fullMarkdownSpec =
  fullMarkdownSpecSansPipeTable
    -- as the commonmark documentation states, pipeTableSpec should be placed after
    -- fancyListSpec and defaultSyntaxSpec to avoid bad results when parsing
    -- non-table lines
    <> CE.pipeTableSpec

pipeTableSpec ::
  (Monad m, CM.IsBlock il bl, CE.HasPipeTable il bl) =>
  CM.SyntaxSpec m il bl
pipeTableSpec = CE.pipeTableSpec

fullMarkdownSpecSansPipeTable ::
  SyntaxSpec' m il bl =>
  CM.SyntaxSpec m il bl
fullMarkdownSpecSansPipeTable =
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
      CM.defaultSyntaxSpec
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
