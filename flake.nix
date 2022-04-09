{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/c48167590e3258daac6ab12a41bc2b7341e9b2ec";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "parameterized";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          name = "ema";
          pkgs = nixpkgs.legacyPackages.${system};
          hp = pkgs.haskellPackages;
          emaProject = returnShellEnv:
            hp.developPackage {
              inherit name returnShellEnv;
              root = ./.;
              withHoogle = false;
              overrides = self: super: with pkgs.haskell.lib; {
                # lvar = self.callCabal2nix "lvar" inputs.lvar { };
                # url-slug = inputs.url-slug.defaultPackage.${system};
              };
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (with hp; pkgs.lib.lists.optionals returnShellEnv [
                    # Specify your build/dev dependencies here. 
                    cabal-fmt
                    cabal-install
                    ghcid
                    haskell-language-server
                    ormolu
                    pkgs.nixpkgs-fmt
                  ]);
            };

          fourmoluOpts = "-o-XTypeApplications -o-XImportQualifiedPost";

          # Checks the shell script using ShellCheck
          checkedShellScript = name: text:
            (pkgs.writeShellApplication {
              inherit name text;
            }) + "/bin/${name}";

          # Concat a list of Flake apps to produce a new app that runs all of them
          # in sequence.
          concatApps = apps:
            {
              type = "app";
              program = checkedShellScript "concatApps"
                (pkgs.lib.strings.concatMapStringsSep
                  "\n"
                  (app: app.program)
                  apps);
            };
        in
        rec {
          # Used by `nix build`
          defaultPackage = packages.default;
          # Used by `nix run`
          defaultApp = packages.default;
          # Used by `nix develop`
          devShell = emaProject true;

          packages = {
            default = emaProject false;
          };

          # Used by `nix run ...`
          apps = {
            format = concatApps [
              (inputs.lint-utils.apps.${system}.fourmolu fourmoluOpts)
              inputs.lint-utils.apps.${system}.cabal-fmt
              inputs.lint-utils.apps.${system}.nixpkgs-fmt
            ];
          };

          # Used by `nix flake check` (but see next attribute)
          checks = {
            format-haskell = inputs.lint-utils.linters.${system}.fourmolu ./. fourmoluOpts;
            format-cabal = inputs.lint-utils.linters.${system}.cabal-fmt ./.;
            format-nix = inputs.lint-utils.linters.${system}.nixpkgs-fmt ./.;
            hls = checkedShellScript "hls" "${hp.haskell-language-server}/bin/haskell-language-server";
          };

          # We need this hack because `nix flake check` won't work for Haskell
          # projects: https://nixos.wiki/wiki/Import_From_Derivation#IFD_and_Haskell
          #
          # Instead, run: `nix build .#check.x86_64-linux` (replace with your system)
          check =
            pkgs.runCommand "combined-checks"
              {
                checksss = builtins.attrValues self.checks.${system};
              } ''
              echo $checksss
              touch $out
            '';

        }) // {
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
