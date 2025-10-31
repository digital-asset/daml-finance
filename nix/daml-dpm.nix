let
  pkgs = import <nixpkgs> {};
in
pkgs.stdenv.mkDerivation {
  pname = "daml-finance-env";
  version = "1.0.0";

  # Tools you need in your environment
  buildInputs = [
    pkgs.nodejs       
    pkgs.jdk          
    pkgs.git          
    pkgs.curl         # used to install DPM
    pkgs.jq          
    pkgs.yq           
  ];

  dontUnpack = true;
  dontBuild = true;

  installPhase = "true";

  # Shell setup that installs DPM automatically
  shellHook = ''
    echo "Initializing DPM-based Daml environment (no Nix SDK build)"

    # Ensure ~/.local/bin is in PATH (where daml/dpm are installed)
    export PATH=$HOME/.local/bin:$PATH

    # Install DPM if not present
    if ! command -v dpm >/dev/null 2>&1; then
      echo "Installing DPM..."
      curl -fsSL https://get.daml.com | sh
      daml install dpm
    fi

    echo "Using DPM version:"
    dpm --v

    echo ""
    echo "You can now run:"
    echo "  dpm build   # to build all Daml packages"
    echo "  dpm test    # to run tests"
    echo ""
  '';
}
