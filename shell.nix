{pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "pigggies-finance-tracker-env";
  buildInputs = with pkgs; [ zlib glpk pcre stack ];
}
