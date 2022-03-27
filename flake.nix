{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/30d3d79b7d3607d56546dd2a6b49e156ba0ec634";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          name = "ema";
          pkgs = nixpkgs.legacyPackages.${system};
          emaProject = returnShellEnv:
            pkgs.haskellPackages.developPackage {
              inherit name returnShellEnv;
              root = ./.;
              withHoogle = false;
              overrides = self: super: with pkgs.haskell.lib; {
                # lvar = self.callCabal2nix "lvar" inputs.lvar { };
                # url-slug = inputs.url-slug.defaultPackage.${system};
                relude = self.relude_1_0_0_1; # relude 1.0 is not used by default in nixpkgs
              };
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (with pkgs.haskellPackages; pkgs.lib.lists.optionals returnShellEnv [
                    # Specify your build/dev dependencies here. 
                    cabal-fmt
                    cabal-install
                    ghcid
                    haskell-language-server
                    ormolu
                    pkgs.nixpkgs-fmt
                  ]);
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

        }) // {
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
