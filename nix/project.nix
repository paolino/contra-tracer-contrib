{ pkgs, self, CHaP }:

let
  indexState = "2025-12-07T00:00:00Z";
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

  # Project with CHaP's contra-tracer (simple function-based API)
  projectIohk = pkgs.haskell-nix.cabalProject' {
    src = ../.;
    compiler-nix-name = "ghc984";
    index-state = indexState;
    shell = shell { inherit pkgs; };
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
    cabalProjectLocal = ''
      repository cardano-haskell-packages
        url: https://chap.intersectmbo.org/
        secure: True

      constraints: contra-tracer < 0.2
    '';
  };

  # Project with Hackage's contra-tracer (arrow-based API)
  projectHackage = pkgs.haskell-nix.cabalProject' {
    src = ../.;
    compiler-nix-name = "ghc984";
    index-state = indexState;
    shell = shell { inherit pkgs; };
    # No inputMap - uses Hackage only
  };

in {
  packages = {
    # Hackage (default)
    lib = projectHackage.hsPkgs.contra-tracer-contrib.components.library;
    unit-tests =
      projectHackage.hsPkgs.contra-tracer-contrib.components.tests.unit-tests;
    # IOHK/CHaP
    lib-iohk = projectIohk.hsPkgs.contra-tracer-contrib.components.library;
    unit-tests-iohk =
      projectIohk.hsPkgs.contra-tracer-contrib.components.tests.unit-tests;
  };

  devShells = {
    default = projectHackage.shell;
    iohk = projectIohk.shell;
  };

  checks = projectHackage.flake'.checks;
}
