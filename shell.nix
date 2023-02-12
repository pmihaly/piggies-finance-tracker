{pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "pigggies-finance-tracker-env";
  buildInputs =
  let
    hPkgs = pkgs.haskell.packages."ghc924";
  in
    with hPkgs; [
      stack
      hlint
      hoogle
      retrie
      # compilation fails with dies with error: cycle detected in build of '...' in the references of output 'bin' from output 'out'
      # haskell-language-server
      # ormolu
      # ghcid
    ];
}
