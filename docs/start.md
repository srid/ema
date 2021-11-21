---
order: 1
---

# Getting Started

As first steps, perform the following before proceeding to the tutorial section below:

1. [Install Nix](https://nixos.org/download.html)[^nix] and [Enable Flakes](https://nixos.wiki/wiki/Flakes#Installing_flakes)
1. Clone [the template repository][ema-template] locally
1. Run `bin/run` and access the site at <http://localhost:9001>

(If you are using `cabal`, rather than Nix - run `ghcid` in place of `bin/run`).

That should start the Ema dev server displaying a simple website. Go ahead and try modifying either the Markdown content in `./content` or the Haskell source in `./src/Main.hs`, and observe how the web view updates [instantly](concepts/hot-reload.md).

{.last}
[Next]{.next}, [in the tutorial](start/tutorial.md) let's try using this template repo to create a basic website.

[^nix]: Nix is optional. The [template repository][ema-template] will work with plain `cabal`.

[ema-template]: https://github.com/srid/ema-template
