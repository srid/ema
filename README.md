# ema

<img width="10%" src="./docs/ema.svg">

Ema is a next-gen **Haskell** library for building [jamstack-style](https://jamstack.org/) static sites, with fast hot reload. See [ema.srid.ca](https://ema.srid.ca/) for documentation.

The simplest Ema app looks like this:

```haskell
main :: IO ()
main = do
  let name :: Text = "Ema"
  runEmaPure $
    encodeUtf8 $ "<b>Hello</b>, from " <> name
```

## Hacking

Run `bin/run` (or <kbd>Ctrl+Shift+B</kbd> in VSCode). This runs the documentation example; modify `./.ghcid` to run a different example, such as the clock example - which updates every second, demonstrating hot reload.

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
    - Expose it to apps (inc/ helpers) in a simple way
- [x] [deal with errors](https://github.com/srid/memoir/issues/1)
- [x] How to serve non-generated files (css, img, etc.)
- [ ] Publish Data.LVar to Hackage
- [ ] documentation ([guide](https://documentation.divio.com/))
  - [x] Avoid tailwind CDN in docs (use twind or windicss) for better lighthouse score
