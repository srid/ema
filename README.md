# ema

**WIP**

ema is the next-generation static site generator and live server that hot-reload's on code *or* data change.

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
