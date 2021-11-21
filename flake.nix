{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/2606cb0fc24e65f489b7d9fdcbf219756e45db35";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    #lvar = {
    #  url = "github:srid/lvar";
    #  flake = false;
    #};
    unionmount = {
      url = "github:srid/unionmount";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
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
              unionmount = self.callCabal2nix "unionmount" inputs.unionmount { };
            };
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
        ema = emaProject false;
      in
      rec {
        # Used by `nix build`
        defaultPackage = ema;

        # Used by `nix develop`
        devShell = emaProject true;
      });
}
