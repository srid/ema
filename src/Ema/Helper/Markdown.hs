{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Helper to deal with Markdown files
--
-- TODO: Publish this to hackage as `Text.Markdown.Simple`?
module Ema.Helper.Markdown
  ( -- Parsing
    parseMarkdownWithFrontMatter,
    parseMarkdown,
    -- Utilities
    plainify,
    -- TODO: Move to different module or package
    fullMarkdownSpec,
    wikilinksSpec,
    wikiLinkSpec,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Inlines as CM
import qualified Commonmark.Pandoc as CP
import qualified Commonmark.TokParsers as CT
import Control.Monad.Combinators (manyTill)
import qualified Data.YAML as Y
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W
import qualified Text.Parsec as P

-- | Parse a Markdown file using commonmark-hs with all extensions enabled
parseMarkdownWithFrontMatter ::
  forall meta.
  (Y.FromYAML meta) =>
  (forall m il bl. SyntaxSpec' m il bl => CM.SyntaxSpec m il bl) ->
  -- | Path to file associated with this Markdown
  FilePath ->
  -- | Markdown text to parse
  Text ->
  Either Text (Maybe meta, Pandoc)
parseMarkdownWithFrontMatter spec fn s = do
  (mMeta, markdown) <- partitionMarkdown fn s
  mMetaVal <- parseYaml fn `traverse` mMeta
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

-- TODO: Probably not a good idea to force users to deal with folgezettels?
data WikiLinkType
  = -- | [[Foo]]
    WikiLinkNormal
  | -- | [[Foo]]#
    WikiLinkBranch
  | -- | #[[Foo]]
    WikiLinkTag
  deriving (Eq, Show)

class HasWikilinks il where
  wikilink :: Text -> il -> il

instance CM.Rangeable (CM.Html a) => HasWikilinks (CM.Html a) where
  wikilink url il = CM.link url "wikilink" il

instance
  (HasWikilinks il, Semigroup il, Monoid il) =>
  HasWikilinks (CM.WithSourceMap il)
  where
  wikilink url il = (wikilink url <$> il) <* CM.addName "wikilink"

-- | Like `Commonmark.Extensions.Wikilinks.wikilinksSpec` but Zettelkasten-friendly.
--
-- Compared with the official extension, this has two differences:
--
-- - Supports flipped inner text, eg: `[[Foo | some inner text]]`
-- - Supports neuron folgezettel, i.e.: #[[Foo]] or [[Foo]]#
wikilinksSpec ::
  (Monad m, CM.IsInline il, HasWikilinks il) =>
  CM.SyntaxSpec m il bl
wikilinksSpec =
  mempty
    { CM.syntaxInlineParsers = [pWikilink]
    }
  where
    pWikilink = do
      replicateM_ 2 $ CT.symbol '['
      P.notFollowedBy (CT.symbol '[')
      title <-
        CM.untokenize
          <$> many
            ( CT.satisfyTok
                ( \t ->
                    not (CT.hasType (CM.Symbol '|') t || CT.hasType (CM.Symbol ']') t)
                )
            )
      url <-
        M.option title $
          CM.untokenize
            <$> ( CT.symbol '|'
                    *> many (CT.satisfyTok (not . CT.hasType (CM.Symbol ']')))
                )
      replicateM_ 2 $ CT.symbol ']'
      return $ wikilink url (CM.str title)

-- | Commonmark parser extension for wikilinks
--
-- Convert `[[Foo]]` into `[Foo](Foo.md)`. In addition the link's title is set
-- to the `show` value of `WikiLinkType` for later decoding.
wikiLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
wikiLinkSpec =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink ::
      (Monad m, CM.IsInline il) =>
      CM.InlineParser m il
    pLink =
      P.try $
        P.choice
          [ cmAutoLink WikiLinkTag
              <$> P.try (CT.symbol '#' *> wikiLinkP),
            cmAutoLink WikiLinkBranch
              <$> P.try (wikiLinkP <* CT.symbol '#'),
            cmAutoLink WikiLinkNormal
              <$> P.try wikiLinkP
          ]
    wikiLinkP :: Monad m => P.ParsecT [CM.Tok] s m Text
    wikiLinkP = do
      let open = '['
          close = ']'
          parenP = replicateM_ 2 . CT.symbol
          innerP = fmap CM.untokenize $ some $ CT.noneOfToks [CM.Symbol open, CM.Symbol close, CM.LineEnd]
      parenP '[' *> innerP <* parenP ']'
    cmAutoLink :: CM.IsInline a => WikiLinkType -> Text -> a
    cmAutoLink typ wikiLinkText =
      CM.link url title $ CM.str wikiLinkText
      where
        -- Store connetion type in 'title' attribute for later lookup.
        -- TODO: Put it in attrs instead; requires PR to commonmark
        title = show typ
        -- If [[Foo]], use url Foo.md
        url = wikiLinkText <> ".md"
