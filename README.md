# ema

ema is a **WIP** next-gen Haskell static site generator that is *change-aware*. In addition to static site generation, it provides a live server that hot-reload's on code *or* data change[^1]. 

The ultimate goal of ema is to make it possible to easily implement your own [neuron](https://neuron.zettel.page/), or just about any app that creates a browser view of arbitrarily changing data (on disk, database, or whatever). ema is designed to facilitate creation of apps whose data is normally *edited* via traditional mechanisms (eg: text editor) but *rendered* as a delightful web page - so as to provide an economical read-only view, of your data, on desktop & mobile.

The simplest ema app looks like this:

```haskell
main :: IO ()
main = do
  let name :: Text = "Srid"
  runEmaPure $ \() ->
    encodeUtf8 $ "<b>Hello,</b> " <> name
```

[^1]: At the moment, only data change triggers a true hot reload; but code change triggers reload via a full page refresh in the browser, which creates a subtle flicker. Ideally this should be improved somehow, possibly by persisting the server and websocket across ghci(d) restarts (cf. [ghci-websockets](https://github.com/j-mueller/ghci-websockets)).

## Hacking

Open in VSCode, and run the build task.

## TODO

- [x] MVP
- [x] Implement hot reload, and ditch browser-sync
  - [x] server to client refresh
  - [x] client to server reconnect (on ghcid reload, or accidental client disconnect)
    - [x] or, investigate https://hackage.haskell.org/package/ghci-websockets
- [ ] Refactor, and consider multi-ws-client support

features,

pre-announce,
- [ ] plan features, re: hakyll
- [ ] CLI UX (opts, logging, etc.)
- [ ] add common examples, including filesystem watcher
- [ ] documentation ([howto](https://documentation.divio.com/))

doc notes,
- use async:race to avoid ghcid ghosts
- at most one ws client supported right now
- tailwind + blaze-html layout (BlazeWind?) for no-frills getting started
