{
  description = "Utility modules for contra-tracer";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskellNix, mkdocs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          project = import ./nix/project.nix {
            pkgs = import nixpkgs {
              overlays = [ haskellNix.overlay ];
              inherit system;
            };
            inherit self;
            inherit (inputs) CHaP;
          };
        in {
          packages = project.packages // {
            default = project.packages.lib;
            docs = mkdocs.lib.${system}.mkDocsSite {
              name = "contra-tracer-contrib-docs";
              src = ./.;
            };
          };

          devShells = project.devShells;

          checks = project.checks;
        };

      flake = { version = self.dirtyShortRev or self.shortRev or "dev"; };
    };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
}
