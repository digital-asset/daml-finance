let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; };
  src = pkgs.lib.cleanSourceWith {
    src = ./.;
    filter = path: type: true;
  };
  packell = pkgs.haskell.packages.ghc8107.callCabal2nix "packell" src {};
in
pkgs.mkShell {
  buildInputs = [
    hls
    packell
    pkgs.file
    pkgs.binutils
    pkgs.ghc
    pkgs.haskellPackages.cabal-install
  ];
  shellHook = ''
    echo "✔ Using LOCAL packell source from: $(pwd)"
  '';
}
