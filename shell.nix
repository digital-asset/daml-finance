# Add new nixpkgs source with (find matching revision here https://lazamar.co.uk/nix-versions/):
# nix-shell -p niv --run "niv add NixOS/nixpkgs -n nixpkgs-ghc8107 -b master -r d1c3fea7ecbed758168787fe4e4a3157e52bc808"
# Update nixpkgs with:
# nix-shell -p niv --run "niv update"

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  pkgsGhc = import sources.nixpkgs-ghc8107 {};
  daml = import ./nix/daml.nix;
  packell = import ./nix/packell.nix;
  damlYaml = builtins.fromJSON (builtins.readFile (pkgs.runCommand "daml.yaml.json" { yamlFile = ./daml.yaml; } ''
                ${pkgs.yj}/bin/yj < "$yamlFile" > $out
              ''));
in
pkgs.mkShell {
  SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  buildInputs = [
    (daml { stdenv = pkgs.stdenv;
            jdk = pkgs.openjdk11_headless;
            sdkVersion = damlYaml.sdk-version;
            damlVersion = damlYaml.daml-version;
            curl = pkgs.curl;
            curl_cert = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
            os = if pkgs.stdenv.isDarwin then "macos" else "linux";
            use_ee = true;
            hashes = { linux = "sha256-LsziR0wtHYqs7PVKGk1FI6xs4l3LbwbCgGgfYn7p+sk";
                       macos = "sha256-Jyfl1wfIiNtXcnOxqX94CYptzi8A4EZEA7330uItWBM="; };})
    (packell { pkgs = pkgsGhc; stdenv = pkgsGhc.stdenv; version = "0.0.2"; })
    pkgs.bash
    pkgs.binutils # cp, grep, etc.
    pkgs.cacert
    pkgs.circleci-cli
    pkgs.curl
    pkgs.gh
    pkgs.git
    pkgs.gnupg
    pkgs.jq
    pkgs.python39
    pkgs.openssh
    pkgs.yq-go]
  ;
}
