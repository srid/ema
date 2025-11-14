{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.flake = false;
    emanote.url = "github:srid/emanote";

    lvar.url = "github:srid/lvar/0.2.0.0";
    lvar.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.fourmolu-nix.flakeModule
        (inputs.git-hooks + /flake-module.nix)
        inputs.emanote.flakeModule
      ];
      perSystem = { config, pkgs, ... }: {
        # This attr is provided by https://github.com/srid/haskell-flake
        haskellProjects.default = {
          autoWire = [ "packages" "checks" ];
          packages.lvar.source = inputs.lvar;
        };

        devShells.default = pkgs.mkShell {
          name = "ema-shell";
          inputsFrom = [
            config.pre-commit.devShell
            config.haskellProjects.default.outputs.devShell
          ];
          packages = with pkgs; [
            nixd
            just
          ];
        };

        pre-commit.settings = {
          hooks = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            fourmolu = {
              enable = true;
              package = config.fourmolu.wrapper;
              excludes = [
                "vira\.hs"
              ];
            };
          };
        };

        fourmolu.settings = {
          indentation = 2;
          comma-style = "leading";
          record-brace-space = true;
          indent-wheres = true;
          import-export-style = "diff-friendly";
          respectful = true;
          haddock-style = "multi-line";
          newlines-between-decls = 1;
          extensions = [ "ImportQualifiedPost" ];
        };

        emanote = {
          sites = {
            "docs" = {
              layers = [
                {
                  path = ./docs;
                  pathString = "./docs";
                }
              ];
              prettyUrls = true;
            };
          };
        };
      };
    };
}
