{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/2ea2f7b6d0cb7ce0712f2aa80303cda08deb0de2";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "overengineered";
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

          lintSpec = {
            nixpkgs-fmt = { };
            cabal-fmt = { };
            fourmolu = {
              ghcOpts = "-o-XTypeApplications -o-XImportQualifiedPost";
            };
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
            format = inputs.lint-utils.mkApp.${system} lintSpec;
          };

          # Used by `nix flake check` (but see next attribute)
          checks = {
            format = inputs.lint-utils.mkChecks.${system} lintSpec;
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
