{ stdenv, jdk, curl, curl_cert, version, os, hashes }:
let
  tarball = stdenv.mkDerivation {
    pname = "daml-tarball";
    version = version;
    src = ./..;
    buildInputs = [ curl ];
    SSL_CERT_FILE = curl_cert;
    impureEnvVars = [ "ARTIFACTORY_USERNAME" "ARTIFACTORY_PASSWORD" ];
    buildPhase = ''
      set -euo pipefail

      if [ -n "''${ARTIFACTORY_PASSWORD:-}" ]; then
        curl -u $ARTIFACTORY_USERNAME:$ARTIFACTORY_PASSWORD \
             https://digitalasset.jfrog.io/artifactory/assembly/daml/${version}/daml-sdk-${version}-${os}.tar.gz \
          > $out
      else
        echo "ARTIFACTORY_USERNAME and ARTIFACTORY_PASSWORD must be set." >&2
        exit 1
      fi
    '';
    dontInstall = true;
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = hashes.${os};
  };
in
  stdenv.mkDerivation {
    pname = "daml-sdk";
    version = version;
    src = tarball;
    dontUnpack = true;
    buildPhase = ''
      mkdir daml
      tar xzf $src -C daml --strip-components 1
      patchShebangs .
    '';
    installPhase = "cd daml; DAML_HOME=$out ./install.sh";
    propagatedBuildInputs = [ jdk ];
    preFixup = ''
      # Set DAML_HOME automatically.
      mkdir -p $out/nix-support
      echo export DAML_HOME=$out > $out/nix-support/setup-hook
    '';
  }
