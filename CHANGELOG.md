# Revision history for ema

## Unreleased (0.2.0.0)

- `Ema.Slug`
  - Add `Ord` instance to `Slug`
  - Unicode normalize slugs using NFC
  - Add `decodeSlug` and `encodeSlug`
- Add default implementation based on Enum for `staticRoute`
- Warn, without failing, on missing `staticAssets` during static generation
- Helpers
  - Helpers.FileSystem
  - add `mountOnLVar`
  - Helpers.Tailwind
    - add overflow-y-scroll to body
    - Add twind shim *before* application's head
  - Helpers.Markdown (to be moved to Hackage eventually)
    - add helpers to parse markdown; `parseMarkdownWithFrontMatter` and `parseMarkdown`
    - add wikilink helpers
  - Add `Ema.Helper.PathTree`
- Examples
  - Remove Ex03_Documentation.hs (moved to separate repo, `ema-docs`)
  - Add Ex03_Basic.hs example

## 0.1.0.0 -- 2021-04-26

* First version. Released on an unsuspecting world.
