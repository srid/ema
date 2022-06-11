---
order: 1
---

# Getting Started

Before proceeding with the tutorial section below, let's get the template app up and running locally:

1. [Install Nix](https://nixos.org/download.html)[^nix] and [enable Flakes](https://nixos.wiki/wiki/Flakes#Installing_flakes)
1. Clone [the template repository][ema-template] locally
1. Run `bin/run`[^cabal] and access the site at <http://localhost:9001>

[^cabal]: If you are using `cabal` rather than Nix, run `ghcid` in place of `bin/run`.

That should start the Ema dev server displaying a simple website. Go ahead and try modifying either the Markdown content in `./content` or the Haskell source in `./src/Main.hs`, and observe how the web view updates [instantly](concepts/hot-reload.md).

{.last}
[Next]{.next}, [in the tutorial](start/tutorial.md) let's try using this template repo to create a basic website.

[^nix]: Nix is optional, though it provides the best development experience. The [template repository][ema-template] may work with plain `cabal`, but this is not officially tested.

[ema-template]: https://github.com/srid/ema-template
