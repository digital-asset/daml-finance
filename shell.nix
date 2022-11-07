# Add new nixpkgs source with (find matching revision here https://lazamar.co.uk/nix-versions/):
# nix-shell -p niv --run "niv add NixOS/nixpkgs -n nixpkgs-ghc8107 -b master -r d1c3fea7ecbed758168787fe4e4a3157e52bc808"
# Update nixpkgs with:
# nix-shell -p niv --run "niv update"

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  pkgsGhc = import sources.nixpkgs-ghc8107 {};
  daml = import ./nix/daml.nix;
  docs = import ./nix/docs.nix;
  haskell = import ./tools/packell/default.nix;
  damlYaml = builtins.fromJSON (builtins.readFile (pkgs.runCommand "daml.yaml.json" { yamlFile = ./daml.yaml; } ''
                ${pkgs.yj}/bin/yj < "$yamlFile" > $out
              ''));
in
pkgs.mkShell {
  SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  buildInputs = with pkgs; [
    (daml { stdenv = stdenv;
            jdk = openjdk11_headless;
            version = damlYaml.sdk-version; })
    bash
    binutils # cp, grep, etc.
    cacert
    circleci-cli
    curl
    gh
    git
    gnupg
    jq
    yq-go]
    ++ (docs { pkgs = pkgs; })
    ++ (haskell { pkgs = pkgsGhc; })
  ;
}
