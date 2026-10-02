{ pkgs ? import <nixpkgs> {} }:

let
  dpmPath = "europe-docker.pkg.dev/da-images/public/components/dpm";
  dpmVersion = "1.0.2";
  dpmRef = "${dpmPath}:${dpmVersion}";

  dpmHashes = {
    "x86_64-linux" = "sha256-DLe6KrEUcXxrmO0kNJRehV/x4BGVnN6IdxMb5tf45nE=";
    "aarch64-linux" = "sha256-MpF5B/SmZ6BlioEju64f9dABCeUoa/gOyqsvb5MdYZg=";
    "x86_64-darwin" = "sha256-FIbh1RqM0xHDL7pzA2ard3xmOnQ78BOBj+fEre6hiaE=";
    "aarch64-darwin" = "sha256-IjIljDMUkxcI38ODUpl+EbqfRm5bknA9agyijeIbWKU=";
  };
  dpmHash = dpmHashes.${pkgs.system} or (throw "Unsupported system: ${pkgs.system}");

  ociPlatforms = {
    "x86_64-linux" = "linux/amd64";
    "aarch64-linux" = "linux/arm64";
    "x86_64-darwin" = "darwin/amd64";
    "aarch64-darwin" = "darwin/arm64";
  };
  ociPlatform = ociPlatforms.${pkgs.system} or (throw "Unsupported system: ${pkgs.system}");
in
pkgs.stdenv.mkDerivation {
  pname = "dpm";
  version = dpmVersion;

  src = pkgs.stdenv.mkDerivation {
    name = "dpm-pull-${dpmRef}.${ociPlatform}";

    nativeBuildInputs = [ pkgs.oras pkgs.cacert ];

    buildCommand = ''
      set -e
      echo "Pulling dpm component from ${dpmRef} for platform ${ociPlatform}"
      oras pull --platform ${ociPlatform} -o $out ${dpmRef}
    '';
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
    outputHash = dpmHash;
  };

  installPhase = ''
    mkdir -p $out/bin
    install -Dm755 $src/dpm $out/bin/dpm
  '';
}
