{ pkgs ? import ./nix/sources.nix {} }:

let
  src = pkgs.lib.cleanSourceWith {
    src = ./.;
    filter = path: type: true;
  };
in
pkgs.haskell.packages.ghc8107.callCabal2nix "packell" src {}
