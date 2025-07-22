# Revision history for ema

## 0.12.0.0 (2025-07-22)

- Relax `base` constraint forever
- Require `lvar` 0.2 or later ([simplified API](https://github.com/srid/lvar/pull/8))
- API changes
  - `Ema.CLI`: The `Action` type is no longer a GADT.
  - `Ema.Server`: This module has be split into several smaller modules
- Live server:
  - Shim/websocket customization ([\#152](https://github.com/srid/ema/pull/152)) @lucasvreis
  - Fix scrolling to page end when using pathname in anchor links (\#162)
  - Add `--no-ws` to disable websocket handling in live server (\#161)
  - Switch from `morphdom` to `idiomorph`
  - Allow skipping recreation of any `<script>` tag during live server update by using `data-ema-skip=true` custom attribute (\#170)

## 0.10.2.0 (2023-08-09)

- Simply websocket route observation logic (\#154)

## 0.10.0.0 (2022-11-20)

- Split Ema into multiple packages:
- `ema-extra`: New API
  - Expose `StringRoute` from examples to ema-extra
  - Add `MapRoute` and `PaginatedRoute`

## 0.8.2.0 (2022-08-28)

- Bug fixes
  - Fix live server redirecting static assets to wrong links [\#138](https://github.com/EmaApps/ema/issues/138)

## 0.8.0.0 (2022-08-19)

This releases brings a significant rewrite of Ema. If you choose to upgrade your apps from 0.6, see https://ema.srid.ca/start/upgrade for guidance.

- GHC 9.x support
- Better handling of URL anchors (#83; #87)
- `routeUrl` uses `UrlDirect` by default. Use `routeUrlWith` if you want to change that.
- Ema status indicator now works independently (requires no Tailwind)
- Multisite rewrite (Ema has been mostly rewritten for better composability) ([\#82](https://github.com/EmaApps/ema/pull/81))
  - Represent route encoding using `Prism'` from optics-core; add `IsRoute` class to define them.
    - Optional generic deriving of route prisms, so you do not have to hand-write them.
    - Automatic isomorphism checks ensures that encoding and decoding are isomorphic (route prisms are lawful)
  - Composable Ema apps
    - There are two ways of composing Ema apps. Using heterogenous lists (see `Ema.Route.Lib.Multi`), or by defining a top-level route type (see `Ex04_Multi.hs`).
  - Replace `LVar` with `Dynamic`.
    - Ema still uses `LVar` internally (for live server updates), but on the user-side one only needs to provide a `Dynamic` which is a tuple of initial value and an updating function. The [unionmount](https://github.com/srid/unionmount/pull/1) library was changed to provide this tuple.
  - Add `EmaSite` typeclass to "connect them all"
    - `SiteArg`: Type of value to pass from the environment.
    - `siteInput`: Define the `Dynamic` model for the site.
    - `siteOutput`: Asset (eg: HTML) to produce for each route.

## 0.6.0.0 -- 2022-02-05

- Websocket API: Add `ema.switchRoute` to switch to other routes in live server.
- Smaller core: remove helpers and examples (examples can be enabled by a flag)
  - Helpers
    - `Ema.Helpers.PathTree` moved to separate package *pathtree*.
    - `Ema.Helpers.FileSystem` moved to separate package *unionmount*.
    - `Ema.Helpers.Markdown` moved to separate package *commonmark-simple*.
    - `Ema.Helpers.Blaze` is no more. See `ema-template` if you need a ready made template using blaze HTML and TailwindCSS.
  - Examples `with-examples` cabal flag is made False by default. Examples are not exposed modules.
  - `Ema.Route.Slug` moved to separate package *url-slug*

## 0.4.0.0 -- 2022-01-19

- Pin TailwindCSS to 2.x, because the 3.x broke our CDN url
- Remove unused Cabal deps (#61)
- `Tailwind.layoutWith`: don't hardcode `<body>` attrs
- Tailwind: module revamped and renamed to `Tailwind.Helper.Blaze`
- `runEma` and friends:
  - return the monadic's action's return value or generated files (dependent type)
- CLI: add `run` subcommand that takes `--host` and `--port` (and remove environment hacks of $HOST and $PORT)

## 0.2.0.0 -- 2021-11-21

- TODO(doc) runEma's action gets the `CLI.Action` as argument, to prevent the `gen` command from needing to monitoring files.
- Live Server
  - Avoid unncessary DOM patch on page load
  - Handle invalid routes gracefully without breaking websocket
  - Re-add `<script>` tags on hot reload
  - Scroll to top on route switches
  - Use secure websockets when on HTTPS
  - Bind to loopback (127.0.0.1) for security reasons
  - Do not handle target=_blank links in websocket route switch
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
    - Add Union mount support; re-exported from `unionmount` library
    - enrich FileAction type to distinguish between existance and new and update states
  - Helpers.Tailwind
    - add overflow-y-scroll to body
    - Add twind shim *before* application's head
    - CDN: Use latest version always.
  - Helpers.Markdown
    - add helpers to parse markdown; `parseMarkdownWithFrontMatter` and `parseMarkdown`
- Examples
  - ~~Remove Ex03_Documentation.hs (moved to separate repo, `ema-docs`)~~ Back to ./docs, but using Emanote.
  - Add Ex03_Basic.hs example

## 0.1.0.0 -- 2021-04-26

* First version. Released on an unsuspecting world.
