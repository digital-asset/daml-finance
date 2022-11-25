{ pkgs }:

let
  hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; };
  src = builtins.path { path = ./.; name = "packell"; };
  packell = pkgs.haskell.packages.ghc8107.callCabal2nix "packell" src {};
in
  [
    hls
    packell
    pkgs.ghc
    pkgs.haskellPackages.cabal-install
  ]
