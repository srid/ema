{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    check-flake.url = "github:srid/check-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.check-flake.flakeModule
      ];
      perSystem = { config, pkgs, ... }: {
        # This attr is provided by https://github.com/srid/haskell-flake
        haskellProjects.default = {
          buildTools = hp:
            let
              # Workaround for https://github.com/NixOS/nixpkgs/issues/140774
              fixCyclicReference = drv:
                pkgs.haskell.lib.overrideCabal drv (_: {
                  enableSeparateBinOutput = false;
                });
            in
            {
              treefmt = config.treefmt.build.wrapper;
              ghcid = fixCyclicReference hp.ghcid;
              haskell-language-server = hp.haskell-language-server.overrideScope (lself: lsuper: {
                ormolu = fixCyclicReference hp.ormolu;
              });
            };
          overrides = self: super: with pkgs.haskell.lib; { };
        };

        # treefmt-nix configuration
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
              "--ghc-opt"
              "-XTypeApplications"
            ];
          };
        };
      };
    };
}
