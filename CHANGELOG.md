# Revision history for ema

## Unreleased (0.2.0.0)

- `routeUrl`: now returns relative URLs (ie. without the leading `/`)
  - Use the `<base>` tag to specify an explicit prefix for relative URLs in generated HTML. This way hosting on GitHub Pages without CNAME will continue to have functional links.
- `Ema.Slug`
  - Add `Ord`, `Generic`, `Data` and Aeson instances to `Slug`
  - Unicode normalize slugs using NFC
  - TODO(doc) Add `decodeSlug` and `encodeSlug`
- Unicode normalize `routeUrl` (via `decodeSlug`)
- Add default implementation based on Enum for `staticRoute`
- Warn, without failing, on missing `staticAssets` during static generation
- Helpers
  - Helpers.FileSystem
    - add `mountOnLVar`
      - TODO(doc) gracefully handle user exceptions
  - Helpers.Tailwind
    - add overflow-y-scroll to body
    - Add twind shim *before* application's head
  - TODO(doc) Helpers.Markdown (to be moved to Hackage eventually)
    - add helpers to parse markdown; `parseMarkdownWithFrontMatter` and `parseMarkdown`
    - add wikilink helpers
  - TODO(doc) Add `Ema.Helper.PathTree`
- Examples
  - ~~Remove Ex03_Documentation.hs (moved to separate repo, `ema-docs`)~~ Back to ./docs, but using Emanote.
  - Add Ex03_Basic.hs example

## 0.1.0.0 -- 2021-04-26

* First version. Released on an unsuspecting world.
