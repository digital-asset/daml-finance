{ stdenv, jdk, curl, curl_cert, sdkVersion, damlVersion, os, hashes }:
let
  tarball = stdenv.mkDerivation {
    pname = "daml-tarball";
    version = sdkVersion;
    src = ../.;
    buildInputs = [ curl ];
    SSL_CERT_FILE = curl_cert;
    impureEnvVars = [ "ARTIFACTORY_USERNAME" "ARTIFACTORY_PASSWORD" ];
    buildPhase = ''
      set -euo pipefail

      if [ -f .envrc.private ]; then
        source .envrc.private
      fi

      get_os() (
        curl --location \
             --fail \
             https://github.com/digital-asset/daml/releases/download/v${damlVersion}/daml-sdk-2.9.0-snapshot.20240619.12850.0.v0cfddd39-${os}.tar.gz \
          > $out
      )
      get_ee() (
        if [ -n "''${ARTIFACTORY_PASSWORD:-}" ]; then
          curl -u $ARTIFACTORY_USERNAME:$ARTIFACTORY_PASSWORD \
               https://digitalasset.jfrog.io/artifactory/assembly/daml/${sdkVersion}/daml-sdk-2.9.0-snapshot.20240619.12850.0.v0cfddd39-${os}.tar.gz \
            > $out
        else
          echo "ARTIFACTORY_USERNAME and ARTIFACTORY_PASSWORD must be set." >&2
          exit 1
        fi
      )

      get_os || get_ee
    '';
    dontInstall = true;
    outputHashAlgo = "sha256";
    outputHashMode = "flat";
    outputHash = hashes.${os};
  };
in
  stdenv.mkDerivation {
    pname = "daml-sdk";
    version = sdkVersion;
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
