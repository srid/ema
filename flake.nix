{
  description = "Ema project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    # 1.1 not in nixpkgs or cabal hases yet 
    relude.url = "github:kowainik/relude/v1.1.0.0";
    relude.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { pkgs, ... }: {
        # This attr is provided by https://github.com/srid/haskell-flake
        haskellProjects.default = {
          root = ./.;
          haskellPackages = pkgs.haskell.packages.ghc922; # Needed for `UnconsSymbol`
          buildTools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt;
            inherit (hp)
              cabal-fmt
              ormolu;
          };
          source-overrides = {
            inherit (inputs) relude;
          };
        };
      };
    };
}
