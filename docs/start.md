---
order: 1
---

# Getting Started

The best way to get started with Ema is via **Nix**,[^nix] using the template repository:

1. [Install Nix](https://nixos.org/download.html)[^nix] and [enable Flakes](https://nixos.wiki/wiki/Flakes#Installing_flakes)
1. Clone [the template repository][ema-template] locally
1. Run `bin/run` and access the site at <http://localhost:9001>

Step 3 should start the Ema [[live-server]] displaying a simple website. Go ahead and try modifying the HTML DSL in the Haskell source `./src/Main.hs`, and observe how the browser view updates [[hot-reload|instantly]].

{.last}
[Next]{.next}, [[tutorial|in the tutorial series]] we will start from scratch a trivial site and evolve it towards a feature-rich one.

[^nix]: Nix is optional. However, it provides the best development experience. The [template repository][ema-template] may work with plain `cabal`, but this is not officially tested.

[ema-template]: https://github.com/EmaApps/ema-template
