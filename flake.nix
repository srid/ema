{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/abfd31179174133ab8131139d650297bf4da63b7";
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
                    pkgs.treefmt
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
