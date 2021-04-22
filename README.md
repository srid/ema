# ema

<img width="10%" src="./ema.svg">

ema is a **WIP** next-gen Haskell static site generator that is *change-aware*. In addition to static site generation, it provides a live server that hot-reload's on code *or* data change. 

The ultimate goal of ema is to make it possible to easily implement your own [neuron](https://neuron.zettel.page/), or just about any app that creates a browser view of arbitrarily changing data (on disk, database, or whatever). ema is designed to facilitate creation of apps whose data is normally *edited* via traditional mechanisms (eg: text editor) but *rendered* as a delightful web page - so as to provide an economical read-only view, of your data, on desktop & mobile.

The simplest ema app looks like this:

```haskell
main :: IO ()
main = do
  let name :: Text = "Srid"
  runEmaPure $ \() ->
    encodeUtf8 $ "<b>Hello,</b> " <> name
```

## Hacking

Run `bin/run` (or <kbd>Ctrl+Shift+B</kbd> in VSCode). This runs the clock example; modify `./.ghcid` to run a different example.

## TODO

- [x] MVP
- [x] Implement hot reload, and ditch browser-sync
  - [x] server to client refresh
  - [x] client to server reconnect (on ghcid reload, or accidental client disconnect)
    - [x] or, investigate https://hackage.haskell.org/package/ghci-websockets
- [x] Multi-websocket-client support
- [ ] Static site generation mode
- [ ] add common examples,
  - [x] filesystem watcher
  - [ ] docs site for self (w/ sidebar and possibly even search)

pre-announce,
- [ ] CLI UX (opts, logging, etc.)
- [ ] How to serve non-generated files (css, img, etc.)
- [ ] Publish Data.LVar to Hackage
- [ ] documentation ([howto](https://documentation.divio.com/))

doc notes,
- use async:race to avoid ghcid ghosts
- at most one ws client supported right now
- tailwind + blaze-html layout (BlazeWind?) for no-frills getting started
- [dealing with errors](https://github.com/srid/memoir/issues/1)
- messaging re: hakyll 
  - safer/ simpler routes system
  - bring your own templates / DSL
