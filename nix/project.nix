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
  packages = { lib = project.hsPkgs.contra-tracer-contrib.components.library; };

  devShells.default = project.shell;

  checks = project.flake'.checks;
}
