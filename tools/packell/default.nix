{ pkgs }:

let
  # haskellDeps = ps: with ps; [
  #   aeson
  #   base
  #   colourista
  #   directory
  #   filepath
  #   filepattern
  #   optparse-applicative
  #   text
  #   utf8-string
  #   vector
  #   yaml
  # ];
  # haskellDeps = ps: with ps; [ ];
  # haskellEnv = pkgs.haskell.packages.ghc8107.ghcWithPackages haskellDeps;
  hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; };
  src = builtins.path { path = ./.; name = "packell"; };
  packell = pkgs.haskell.packages.ghc8107.callCabal2nix "packell" src {};
in
  [
    # haskellEnv
    pkgs.ghc
    hls
    pkgs.haskellPackages.cabal-install
    packell
  ]
