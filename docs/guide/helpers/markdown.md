---
order: 3
---

# Using Markdown

There are quite a few packages to convert Markdown to HTML,

- [Pandoc](https://hackage.haskell.org/package/pandoc) -- [Supports formats other than Markdown]{.item-intro}
- [commonmark-hs](https://github.com/jgm/commonmark-hs) -- [Lightweight parser by the same author of Pandoc]{.item-intro}
- [mmark](https://github.com/mmark-md/mmark) -- [*Strict* Markdown parser]{.item-intro}

## Helper

Ema provides a helper to parse Markdown files with YAML frontmatter, using commonmark-hs. If you are parsing front matter, you can use any type that has a [`FromYAML`](https://hackage.haskell.org/package/HsYAML-0.2.1.0/docs/Data-YAML.html#t:FromYAML) instance.

```haskell
import qualified Ema.Helper.Markdown as Markdown

-- Front matter metadata can be any type with a `FromYAML` instance
-- 
-- Using a `Map` is a lazy way to capture metadata, but in real code we
-- generally define a sum type and manually derive `FromYAML` for it.
type Metadata = Map Text Text 

-- Returns `Either Text (Metadata, Pandoc)`
Markdown.parseMarkdownWithFrontMatter @Metadata 
    "test.md" "Hello *world*"
```

The template repo, as well as [Emanote](https://github.com/srid/emanote) (used to generate this site), uses this helper to parse Markdown files into Pandoc AST. Consult [the template repo's source code](https://github.com/srid/ema-template/blob/master/src/Main.hs) for details.

Note that with Ema you can get [hot reload](concepts/hot-reload.md) support for your Markdown files using [filesystem notifications](guide/helpers/filesystem.md).
