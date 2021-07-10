# Revision history for ema

## Unreleased (0.2.0.0)

- TODO(doc) runEma's action gets the `CLI.Action` as argument, to prevent the `gen` command from needing to monitoring files.
- Live Server
  - Avoid unncessary DOM patch on page load
  - Handle invalid routes gracefully without breaking websocket
  - Re-add `<script>` tags on hot reload
  - Scroll to top on route switches
  - Use secure websockets when on HTTPS
- `Asset` type
  - Introduce the `Asset` type to distinguishing between static files and generated files. The later can be one of `Html` or `Other`, allowing the live server to handle them sensibly.
  - `Ema` typeclass: 
    - Drop `staticAssets` in favour of `allRoutes` (renamed from `staticRoutes`) returning all routes including both generated and static routes.
    - Drop `Slug` and use plain `FilePath`. Route encoder and decoder deal directly with the on-disk path of the generated (or static) files.
  - Make the render function (which `runEma` takes) return a `Asset LByteString` instead of `LByteString` such that it can handle all routes, and handle static files as well as generation of non-HTML content (eg: RSS)
  - Allow copying static files anywhere on the filesystem
- `routeUrl`: 
  - Unicode normalize as well URI encode route URLs
  - now returns relative URLs (ie. without the leading `/`)
    - Use the `<base>` tag to specify an explicit prefix for relative URLs in generated HTML. This way hosting on GitHub Pages without CNAME will continue to have functional links.
  - Fix: prevent encoding of non-HTML paths
  - Now takes the `model` type as argument, inasmuch as `encodeRoute` takes it as as well (to accomodate scenarios where route path can only be computed depending on model state; storing slug aliases for instance)
  - Add `routeUrlWith` for non-pretty URLs
- `Ema.Slug`
  - Add `Ord`, `Generic`, `Data` and Aeson instances to `Slug`
  - Unicode normalize slugs using NFC
  - Add `decodeSlug` and `encodeSlug`
- Add default implementation based on Enum for `allRoutes`
- Warn, without failing, on missing static assets during static generation
- Static generation
  - Use block buffering to prevent logging from slowing down site generation
  - Write .nojekyll
- CLI
  - Removed `-C` argument (orthogonal to Ema)
- Helpers
  - Helpers.FileSystem
    - add `mountOnLVar`
      - TODO(doc) gracefully handle user exceptions
  - Helpers.Tailwind
    - add overflow-y-scroll to body
    - Add twind shim *before* application's head
    - CDN: Use latest version always.
  - TODO(doc) Helpers.Markdown (to be moved to Hackage eventually)
    - add helpers to parse markdown; `parseMarkdownWithFrontMatter` and `parseMarkdown`
  - TODO(doc) Add `Ema.Helper.PathTree`
- Examples
  - ~~Remove Ex03_Documentation.hs (moved to separate repo, `ema-docs`)~~ Back to ./docs, but using Emanote.
  - Add Ex03_Basic.hs example

## 0.1.0.0 -- 2021-04-26

* First version. Released on an unsuspecting world.
