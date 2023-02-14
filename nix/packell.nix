{ pkgs, stdenv, version }:
let
  platform =
    if stdenv.isDarwin
      then if stdenv.isAarch64 then "aarch64-apple" else "x86_64-apple"
      else "x86_64-linux";
  tarball = fetchTarball {
    url = "https://github.com/digital-asset/daml-finance/releases/download/packell/${version}/packell-${platform}.tar.gz";
  };
  nativeBuildInputs = if stdenv.isLinux then [ pkgs.autoPatchelfHook ] else [ ];
  buildInputs = if stdenv.isLinux then [ pkgs.glibc pkgs.gmp pkgs.libffi] else [ ];
  # pkgs.lib.makeLibraryPath [ pkgs.libffi pkgs.gmp pkgs.glibc ]
in
  stdenv.mkDerivation {
    name = "packell";
    version = "$version";
    src = tarball;
    nativeBuildInputs = nativeBuildInputs;
    buildInputs = buildInputs;
    baseInputs = [ pkgs.binutils ];
    installPhase = ''
      mkdir -p $out/bin
      cp packell $out/bin
    '';
  }

# To run manually, start with this
# patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" packell