# ema

<img width="10%" src="./ema.svg">

Ema is a next-gen **Haskell** library for building [jamstack-style](https://jamstack.org/) static sites, with fast hot reload. See [ema.srid.ca](https://ema.srid.ca/) for documentation.

The simplest Ema app looks like this:

```haskell
main :: IO ()
main = do
  let name :: Text = "Ema"
  runEmaPure $
    encodeUtf8 $ "<b>Hello</b>, from " <> name
```

## Quick Preview

If you have Nix installed with Flakes, give Ema a test-drive by running it to serve its own documentation:

```bash
PORT=8000 nix run github:srid/ema
```

NOTE: This will work if you have this repo's `./docs` directory locally. In future, ema should be made to include the docs in the nix derivation and reference it.

## Hacking

Run `bin/run` (or <kbd>Ctrl+Shift+B</kbd> in VSCode). This runs the clock example; modify `./.ghcid` to run a different example.

## TODO

- [x] MVP
- [x] Implement hot reload, and ditch browser-sync
  - [x] server to client refresh
  - [x] client to server reconnect (on ghcid reload, or accidental client disconnect)
    - [x] or, investigate https://hackage.haskell.org/package/ghci-websockets
- [x] Multi-websocket-client support
- [x] Static site generation mode
- [x] add common examples,
  - [x] filesystem watcher
  - [x] docs site for self (w/ breadcrumbs and possibly even search)

pre-announce,
- CLI UX 
  - [x] opts
  - [ ] logging
- [ ] How to serve non-generated files (css, img, etc.)
- [ ] Publish Data.LVar to Hackage
- [ ] documentation ([howto](https://documentation.divio.com/))
- [ ] Avoid tailwind CDN in docs (use twind or windicss) for better lighthouse score

doc notes,
- hot reload fast, morphdom
  - html templates + tailwind for fast style feedback
- use async:race to avoid ghcid ghosts
- tailwind + blaze-html layout (BlazeWind?) for no-frills getting started
- [dealing with errors](https://github.com/srid/memoir/issues/1)
- messaging re: hakyll 
  - safer/ simpler routes system
  - bring your own templates / DSL
- messaging re: svelte
  - SSE
