{ pkgs }:
let
  dependencies = with pkgs; [ cabal-install
    haskell.compiler.ghc924
  ];
in
  dependencies
