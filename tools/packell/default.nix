{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  haskellDeps = ps: with ps; [
    aeson
    base
    colourista
    directory
    filepath
    filepattern
    optparse-applicative
    text
    utf8-string
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
