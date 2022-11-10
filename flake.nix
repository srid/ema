{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake/order";
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
            packages.ema.root = ./.;
            overrides = self: super: {
              ema = pkgs.haskell.lib.dontCheck super.ema; # test/type-errors requires 9.2
            };
            buildTools = hp: {
              inherit (pkgs)
                treefmt
                nixpkgs-fmt;
              inherit (hp)
                cabal-fmt
                fourmolu;
            };
          };
          ghc92 = {
            packages.ema.root = ./.;
            haskellPackages = pkgs.haskell.packages.ghc924; # Needed for `UnconsSymbol`
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
                inherit (pkgs.haskellPackages)
                  cabal-fmt;
                inherit (hp)
                  fourmolu;
                ghcid = workaround140774 hp.ghcid;
              };
            overrides = self: super: with pkgs.haskell.lib; {
              # All these below are for GHC 9.2 compat.
              relude = dontCheck super.relude_1_1_0_0; # Not the default in nixpkgs yet.
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
