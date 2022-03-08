{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/1fc7212a2c3992eedc6eedf498955c321ad81cc2";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "ema";
        overlays = [ ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        emaProject = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit name returnShellEnv;
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              # lvar = self.callCabal2nix "lvar" inputs.lvar { };
              # url-slug = inputs.url-slug.defaultPackage.${system};
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with pkgs.haskellPackages; [
                  # Specify your build/dev dependencies here. 
                  cabal-fmt
                  cabal-install
                  ghcid
                  haskell-language-server
                  ormolu
                  pkgs.nixpkgs-fmt
                ]);
          };
        ema = emaProject false;
      in
      rec {
        # Used by `nix build`
        defaultPackage = ema;

        checks = {
          pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              nixpkgs-fmt.enable = true;
              ormolu.enable = true;
            };
          };
        };

        # Used by `nix develop`
        devShell = emaProject true;
      });
}
