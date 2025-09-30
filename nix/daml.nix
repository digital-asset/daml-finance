{stdenv, jdk, curl, curl_cert, sdkVersion, damlVersion, tarPath, os, arch, osJFrog, hashes}: #added arch parameter
let
  target_version =
    if arch != "" then "${os}-${arch}" else os;

  nixTarPath =
    if builtins.isPath tarPath || builtins.isString tarPath
    then /. + tarPath   # turn a string or path into a store path
    else tarPath;       # if it’s already a derivation (e.g. fetchurl), just pass it through

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

      get_local() (
        tar_path="${if nixTarPath == null then "" else nixTarPath}"
        if [ -z $tar_path ]; then
          echo "No explict tar set, attempting to download"
          exit 1
        else
          echo "Using explicit tar"
          cp $tar_path $out
          chmod -x $out
        fi
      )
      get_open_source() (
        echo "Downloading SDK from GitHub..."

        sdk_url="https://github.com/digital-asset/daml/releases/download/v${sdkVersion}/daml-sdk-${damlVersion}-${target_version}.tar.gz"
        echo "Downloading from $sdk_url"
        curl --location --fail "$sdk_url" > "$out"
      )
      
      get_enterprise_edition() (
        echo "Downloading SDK from Artifactory..."
        if [ -n "''${ARTIFACTORY_PASSWORD:-}" ]; then
          curl -u $ARTIFACTORY_USERNAME:$ARTIFACTORY_PASSWORD \
               https://digitalasset.jfrog.io/artifactory/assembly/daml/${sdkVersion}/daml-sdk-${sdkVersion}-${osJFrog}.tar.gz \
            > $out
        else
          echo "ARTIFACTORY_USERNAME and ARTIFACTORY_PASSWORD must be set." >&2
          exit 1
        fi
      )

      get_local || get_open_source || get_enterprise_edition
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
    installPhase = ''
      cd daml
      export DAML_HOME=$out
      ./daml/daml install --install-assistant yes --install-with-custom-version $version $src
    '';
    propagatedBuildInputs = [ jdk ];
    preFixup = ''
      # Set DAML_HOME automatically.
      mkdir -p $out/nix-support
      echo export DAML_HOME=$out > $out/nix-support/setup-hook
    '';
  }
