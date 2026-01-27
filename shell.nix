# Add new nixpkgs source with (find matching revision here https://lazamar.co.uk/nix-versions/):
# nix-shell -p niv --run "niv add NixOS/nixpkgs -n nixpkgs-ghc8107 -b master -r d1c3fea7ecbed758168787fe4e4a3157e52bc808"
# Update nixpkgs with:
# nix-shell -p niv --run "niv update"

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  pkgsGhc = import sources.nixpkgs-ghc8107 {};
  build_daml = import ./nix/daml.nix;
  packell = import ./nix/packell.nix;

  #Load custom dpm derivation
  dpm = import ./nix/dpm.nix { inherit pkgs; };

  damlYaml = builtins.fromJSON (builtins.readFile (pkgs.runCommand "daml.yaml.json" { yamlFile = ./daml.yaml; } ''
    ${pkgs.yj}/bin/yj < "$yamlFile" > $out
  ''));

  os =
    if pkgs.stdenv.isDarwin then "macos" else
    if pkgs.stdenv.isLinux then "linux" else
    throw "Unsupported OS";

# Daml SDK on macOS is only available on x86 architecture
  arch =
    if pkgs.stdenv.isDarwin then "x86_64" else   
    if pkgs.stdenv.hostPlatform.system == "x86_64-linux" then "x86_64" else
    if pkgs.stdenv.hostPlatform.system == "aarch64-linux" then "aarch64"
    else ""; #for plain `linux.tar.gz`

  daml = build_daml {
    stdenv = pkgs.stdenv;
    jdk = pkgs.openjdk17_headless;    
    sdkVersion = damlYaml.sdk-version;
    damlVersion = damlYaml.daml-version;
    tarPath = damlYaml.daml-tar-path or null;
    curl = pkgs.curl;
    curl_cert = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    os = os;
    arch = arch;
    osJFrog = "${os}-${arch}";
    hashes = {
                #base64 hashes from update-daml-hashes
      linux = "zPPJJfor22GHpovh2HOJH7AKQLfSW9p0UPgcZCdhSGM=";
      macos = "hITo4qlasMbhuLGfUwGMhuvkwVRaNgWQLdl6mEDx2Ew=";
    };
  };

in
pkgs.mkShell {
  SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

  shellHook = ''
    set -eo pipefail

    export DPM_HOME="$PWD/.dpm"
    SDK_VERSION="${damlYaml.sdk-version}"

    mkdir -p "$DPM_HOME/sdk/dpm-sdk"

    # Make dpm see the SDK without downloading anything:
    if [ ! -e "$DPM_HOME/sdk/dpm-sdk/$SDK_VERSION" ]; then
      ln -s "${daml}/sdk/$SDK_VERSION" "$DPM_HOME/sdk/dpm-sdk/$SDK_VERSION"
    fi
  '';

  buildInputs = [
    daml
    dpm
    (packell { pkgs = pkgsGhc; stdenv = pkgsGhc.stdenv; version = "0.0.3"; })
    pkgs.bash
    pkgs.binutils # cp, grep, etc.
    pkgs.cacert
    pkgs.circleci-cli
    pkgs.curl
    pkgs.gh
    pkgs.git
    pkgs.gnupg
    pkgs.jq
    pkgs.python310
    pkgs.openssh
    pkgs.unixtools.xxd
    pkgs.yq-go
    pkgs.sphinx
  ];
}
