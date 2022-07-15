{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    # 1.1 not in nixpkgs or cabal hashes yet 
    relude.url = "github:kowainik/relude/v1.1.0.0";
    relude.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { config, pkgs, ... }: {
        # This attr is provided by https://github.com/srid/haskell-flake
        haskellProjects = {
          ghc90 = {
            root = ./.;
            buildTools = hp: {
              inherit (pkgs)
                treefmt
                nixpkgs-fmt;
              inherit (hp)
                cabal-fmt
                ormolu;
            };
          };
          ghc92 = {
            root = ./.;
            haskellPackages = pkgs.haskell.packages.ghc923; # Needed for `UnconsSymbol`
            buildTools = hp:
              let
                # https://github.com/NixOS/nixpkgs/issues/140774 reoccurs in GHC 9.2
                workaround140774 = hpkg: with pkgs.haskell.lib;
                  overrideCabal hpkg (drv: {
                    enableSeparateBinOutput = false;
                  });
              in
              {
                inherit (pkgs)
                  treefmt
                  nixpkgs-fmt;
                ormolu = workaround140774 hp.ormolu;
                ghcid = workaround140774 hp.ghcid;
              };
            source-overrides = {
              inherit (inputs) relude;
            };
            overrides = self: super: with pkgs.haskell.lib; {
              # All these below are for GHC 9.2 compat.
              relude = dontCheck super.relude;
              retry = dontCheck super.retry;
              http2 = dontCheck super.http2; # Fails on darwin
              streaming-commons = dontCheck super.streaming-commons; # Fails on darwin
            };
          };
        };
        devShells.default = config.devShells.ghc92;
      };
    };
}
