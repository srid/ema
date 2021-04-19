{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/dcdf30a78a523296b5f9d44fb67afac485b64737";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; };
        emaProject = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "ema";
            root = ./.;
            withHoogle = false;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [
                cabal-install
                cabal-fmt
                pkgs.nixpkgs-fmt
                ghcid
                ormolu
                haskell-language-server
              ]);
          };
      in
      {
        # Used by `nix build`
        defaultPackage = emaProject false;

        # Used by `nix develop`
        devShell = emaProject true;
      });
}
