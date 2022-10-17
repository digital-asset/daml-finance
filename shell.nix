# Update nixpkgs with:
# nix-shell -p niv --run "niv update"

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  daml = import ./nix/daml.nix;
  damlYaml = builtins.fromJSON (builtins.readFile (pkgs.runCommand "daml.yaml.json" { yamlFile = ./daml.yaml; } ''
                ${pkgs.yj}/bin/yj < "$yamlFile" > $out
              ''));
  docs = import ./nix/docs.nix;
in
pkgs.mkShell {
  SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  buildInputs = [
    (daml { stdenv = pkgs.stdenv;
            jdk = pkgs.openjdk11_headless;
            version = damlYaml.sdk-version; })
    pkgs.bash
    pkgs.binutils # cp, grep, etc.
    pkgs.cacert
    pkgs.circleci-cli
    pkgs.curl
    pkgs.gh
    pkgs.git
    pkgs.jq
    pkgs.yq-go
  ] ++ (docs { pkgs = pkgs; });
}
