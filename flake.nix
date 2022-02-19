{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/19574af0af3ffaf7c9e359744ed32556f34536bd";
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
        # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-976899227
        m1MacHsBuildTools =
          pkgs.haskellPackages.override {
            overrides = self: super:
              let
                workaround140774 = hpkg: with pkgs.haskell.lib;
                  overrideCabal hpkg (drv: {
                    enableSeparateBinOutput = false;
                  });
              in
              {
                ghcid = workaround140774 super.ghcid;
                ormolu = workaround140774 super.ormolu;
              };
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
                (with (if system == "aarch64-darwin"
                then m1MacHsBuildTools
                else pkgs.haskellPackages); [
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
