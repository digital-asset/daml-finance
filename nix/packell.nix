# For the Linux version of Packell, the generated executable is 'Dynamically Linked' meaning the
# interpreter and path of the executable is linked to libraries available on the build system at
# build time. To fix this, we can use `autoPatchelfHook` from nix to automatically set the
# interpreter and the 'rpath' of required libraries via 'buildInputs'.
# Note - the Apple compiled Packell builds with everything necessary to run the executable.
#
# See - https://nixos.wiki/wiki/Packaging/Binaries
#     - https://unix.stackexchange.com/questions/522822/different-methods-to-run-a-non-nixos-executable-on-nixos

{ pkgs, stdenv, version }:
let
  platform =
    if stdenv.isDarwin
      then if stdenv.isAarch64 then "aarch64-apple" else "x86_64-apple"
      else "x86_64-linux";
  tarball = fetchTarball {
    url = "https://github.com/digital-asset/daml-finance/releases/download/packell/${version}/packell-${platform}.tar.gz";
  };
in
  stdenv.mkDerivation {
    name = "packell";
    version = "$version";
    src = tarball;
    nativeBuildInputs = if stdenv.isLinux then [ pkgs.autoPatchelfHook ] else [ ];
    buildInputs = if stdenv.isLinux then [  pkgs.gmp pkgs.libffi ] else [ ];
    baseInputs = [ pkgs.binutils ];
    installPhase = ''
      mkdir -p $out/bin
      cp packell $out/bin
    '';
  }
