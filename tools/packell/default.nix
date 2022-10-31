{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  haskellDeps = ps: with ps; [
    base
    directory
    filepath
    filepattern
    # Glob
    lens
    mtl
    optparse-applicative
    random
    vector
    yaml
  ];
  haskellEnv = haskell.packages.ghc8107.ghcWithPackages haskellDeps;
  hls = haskell-language-server.override { supportedGhcVersions = [ "8107"]; };
in mkShell {
  buildInputs = [
    haskellEnv
    haskellPackages.cabal-install
    hls
  ];
}
