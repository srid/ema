# ema

<img width="10%" src="https://ema.srid.ca/favicon.svg">

[![Hackage](https://img.shields.io/hackage/v/ema.svg?logo=haskell)](https://hackage.haskell.org/package/ema)
[![FAIR](https://img.shields.io/badge/FAIR-pledge-blue)](https://www.fairforall.org/about/)

Ema is a next-gen **Haskell** library toolkit for building [jamstack-style](https://jamstack.org/) static sites, with fast hot reload. See [ema.srid.ca](https://ema.srid.ca/) for further information.

https://user-images.githubusercontent.com/3998/116333460-789c1400-a7a1-11eb-8d28-297c349e42c6.mp4

## Hacking

NOTE: We are using GHC 9.2 which is not yet the default in `nixpkgs`, so you may want to use the [garnix cache](https://garnix.io/docs/caching) to avoid long compilation.

Run `bin/run` (or <kbd>Ctrl+Shift+B</kbd> in VSCode). This runs the clock example (which updates every second, only to demonstrate hot reload); modify `./.ghcid` to run a different example. 

## Getting Started

https://ema.srid.ca/start
