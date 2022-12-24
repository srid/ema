# ema

<img width="10%" src="https://ema.srid.ca/favicon.svg">

[![Hackage](https://img.shields.io/hackage/v/ema.svg?logo=haskell)](https://hackage.haskell.org/package/ema)

Ema is a next-gen **Haskell** library toolkit for building [jamstack-style](https://jamstack.org/) static sites, with fast hot reload. See [ema.srid.ca](https://ema.srid.ca/) for further information.

https://user-images.githubusercontent.com/3998/116333460-789c1400-a7a1-11eb-8d28-297c349e42c6.mp4

## Hacking

*NOTE: `flake.nix` uses GHC 9.2 which is not yet the default in `nixpkgs`, so you may want to use [my Nix cache](https://srid.ca/cache.srid.ca) to avoid long compilation times.*

Run `bin/run`. This runs the Ex04_Multi example.

To run the docs, run `nix run github:EmaApps/emanote -- -L ./docs`.

## Getting Started

https://ema.srid.ca/start

## Discussion

To discuss the Ema project, [join Matrix][matrix] or post in [GitHub Discussions][ghdiscuss].

[matrix]: https://matrix.to/#/#ema:matrix.org
[ghdiscuss]: https://github.com/EmaApps/ema/discussions
