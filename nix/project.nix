{ pkgs, self }:

let
  indexState = "2025-01-15T00:00:00Z";
  indexTool = { index-state = indexState; };

  shell = { pkgs, ... }: {
    tools = {
      cabal = indexTool;
      cabal-fmt = indexTool;
      hoogle = indexTool;
      fourmolu = indexTool;
      hlint = indexTool;
      hspec-discover = indexTool;
    };
    buildInputs = [ pkgs.just pkgs.nixfmt-classic pkgs.shellcheck ];
  };

  project = pkgs.haskell-nix.cabalProject' {
    src = ../.;
    compiler-nix-name = "ghc984";
    index-state = indexState;
    shell = shell { inherit pkgs; };
  };

in {
  packages = {
    lib = project.hsPkgs.contra-tracer-contrib.components.library;
    unit-tests =
      project.hsPkgs.contra-tracer-contrib.components.tests.unit-tests;
  };

  devShells.default = project.shell;

  checks = project.flake'.checks;
}
