# For the Linux version of Packell, the compiled executable is 'Dynamically Linked' meaning the
# interpreter and the paths (or 'rpaths') of the compiled executable is linked to
# the interpreter/libraries available on the build system (ie., in its nix store). As the nix paths
# will differ between the build and the host system, running Packell on a host system will generate
# an error due to the differing paths of the interpreter/libraries. To fix this we use
# `autoPatchelfHook` to automatically set both the interpreter and the 'rpath' of the dependending
# libraries used by the executable (depending libraries are set in 'buildInputs').
#
# The Apple compiled Packell ships with everything necessary to run the executable.
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
