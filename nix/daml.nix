{ stdenv, jdk, version }:
let
  os = if stdenv.isDarwin then "macos" else "linux";
  tarball = fetchTarball {
    url = "https://github.com/digital-asset/daml/releases/download/v${version}/daml-sdk-${version}-${os}.tar.gz";
  };
in
  stdenv.mkDerivation {
    name = "daml-sdk";
    version = "$version";
    src = tarball;
    buildPhase = "patchShebangs .";
    installPhase = "DAML_HOME=$out ./install.sh";
    propagatedBuildInputs = [ jdk ];
    preFixup = ''
      # Set DAML_HOME automatically.
      mkdir -p $out/nix-support
      echo export DAML_HOME=$out > $out/nix-support/setup-hook
    '';
}
