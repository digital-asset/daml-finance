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
      linux = "07Pcs/OIYNnkSeK4wPSLz61sAqtA4wEl5oTh8rouhdY=";
      macos = "CNYJOM9SdfnWZfRwyBh5EQq98JHcwzNcmlB2nAL13jE=";
    };
  };

in
pkgs.mkShell {
  SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

  shellHook = ''
    set -euo pipefail
    export DPM_HOME="$PWD/.dpm"
    mkdir -p "$DPM_HOME"

    export PATH="${dpm}/bin:$PATH"

    export XDG_CONFIG_HOME="$PWD/.xdg-config"
    mkdir -p "$XDG_CONFIG_HOME"

    export DOCKER_CONFIG="$PWD/.docker"
    mkdir -p "$DOCKER_CONFIG"
    cat > "$DOCKER_CONFIG/config.json" <<'JSON'
  { "auths": {} }
  JSON

    export DPM_REGISTRY="europe-docker.pkg.dev/da-images/public-all"

    SDK_VERSION="${damlYaml."sdk-version"}"

    if ! dpm version --all -o json 2>/dev/null \
        | ${pkgs.jq}/bin/jq -e --arg v "$SDK_VERSION" \
            'any(.[]; .version == $v and .installed == true)' >/dev/null 2>&1; then
      echo "Installing SDK $SDK_VERSION into $DPM_HOME (registry=$DPM_REGISTRY)..."
      dpm install "$SDK_VERSION"
    fi

    echo "Using dpm: $(command -v dpm || true)"
    echo "DPM_HOME=$DPM_HOME"
    echo "DPM_REGISTRY=$DPM_REGISTRY"
    dpm version
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
