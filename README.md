# ema

ema is a **WIP** next-gen Haskell static site generator that is *change-aware*. In addition to static site generation, it provides a live server that hot-reload's on code *or* data change. 

The ultimate goal of ema is to make it possible to easily implement your own [neuron](https://neuron.zettel.page/), or just about any app that creates a browser view of arbitrarily changing data (on disk, database, or whatever). ema is designed to facilitate creation of apps whose data is normally *edited* via traditional mechanisms (eg: text editor) but *rendered* as a delightful web page, to provide a economical read-only view, of your data, on desktop & mobile.

The simplest ema app looks like this:

```haskell
main :: IO ()
main = do
  let name :: Text = "Srid"
  runEmaPure $ \() ->
    encodeUtf8 $ "<b>Hello,</b> " <> name
```

## Hacking

Open in VSCode, and run the build task.

## TODO

- [x] MVP
- [ ] Implement hot reload, and ditch browser-sync
  - [x] server to client refresh
  - [ ] client to server reconnect (on ghcid reload, or accidental client disconnect)
    - [ ] or, investigate https://hackage.haskell.org/package/ghci-websockets
- [ ] Example: filesystem watcher

public,
- [ ] refactor and simplify
- [ ] examples

doc notes,
- use async:race to avoid ghcid ghosts
- at most one ws client supported right now
- tailwind + blaze-html layout (BlazeWind?) for no-frills getting started
