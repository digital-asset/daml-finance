TODO 

1/ Understand the different versions for both Linux and macos [done]
2/ In nix, get the target CPU architecture (aarch64 vs x86_64) [done]
     - I think we have code examples of this in packell
3/ Recompile daml-ctl with the same version of sdk 3.4.x [done]
     - point to this version fo daml-ctl in daml-finance
4/ Fix up duplicate URL in daml.nix [done]
5/ Ask Canton Team how to get the Contract Keys, that enable flags in 3.4.x [done]
6/ why does my java version go back to "version 11" if I do not put "pkgs.opensdk17" at the very top in "shell.nix" [done]
7/ Check if its normal that I disabled this warning:
        --> i added : "-Wno-template-interface-depends-on-daml-script" in daml-yaml
        " Compiling daml-finance to a DAR.
            File:     src/test/daml
            Hidden:   no
            Range:    1:1-2:1
            Source:   Daml-LF typechecker
            Severity: DsWarning
            Message: 
            warning while type checking package:
            This package defines templates or interfaces, and depends on daml-script.
            Uploading this package to a ledger will also upload daml-script, which will bloat the package store on your participant.
            It is recommended that scripts/tests are defined in a separate package to your templates, and that
            you remove `6b880fca24626115a79dd9f859784a27f0294b9194f21d71adf0d6b1980b3f1f (daml-script, 3.4.0.20250919.14213.0)` from the dependencies in your daml.yaml
            Upgrade this warning to an error -Werror=template-interface-depends-on-daml-script
            Disable this warning entirely with -Wno-template-interface-depends-on-daml-script
"
8/ Check chnage in update-daml-hashes line 64!!!!!
9/ check Daml.Finance.Test.Util.Common allocatePartyByHint part





Release versions
- Create multiple snapshots
- At some point, create a RC (stands for release candidate)
- Offically release the component 

We follow as much as possible sym versioning => https://semver.org/


1) daml-sdk-3.4.0-snapshot.20250919.14213.0.vfa842949-linux-aarch64.tar.gz
2) daml-sdk-3.4.0-snapshot.20250919.14213.0.vfa842949-linux-aarch64.tar.gz.asc
3) daml-sdk-3.4.0-snapshot.20250919.14213.0.vfa842949-linux-x86_64.tar.gz
4) daml-sdk-3.4.0-snapshot.20250919.14213.0.vfa842949-linux-x86_64.tar.gz.asc
5) daml-sdk-3.4.0-snapshot.20250919.14213.0.vfa842949-linux.tar.gz
6) daml-sdk-3.4.0-snapshot.20250919.14213.0.vfa842949-linux.tar.gz.asc
7) daml-sdk-3.4.0-snapshot.20250919.14213.0.vfa842949-macos-x86_64.tar.gz
8) daml-sdk-3.4.0-snapshot.20250919.14213.0.vfa842949-macos-x86_64.tar.g

Os:
    - linux
    - macos

Arch:
    1) Linux:
        - aarch64
        - x86_64
        - "nothing"
    2) macos    
        - x86_64


        #get_open_source() (
        echo "Downloading SDK from GitHub..."
        target_version="${if arch != "" then "${os}-${arch}" else "${os}"}"
        echo "Downloading from https://github.com/digital-asset/daml/releases/download/v${sdkVersion}/daml-sdk-${damlVersion}-$target_version.tar.gz"
        curl --location \
            --fail \
            https://github.com/digital-asset/daml/releases/download/v${sdkVersion}/daml-sdk-${damlVersion}-$target_version.tar.gz \
          > $out
      )


https://github.com/digital-asset/daml/releases/download/v3.4.0-snapshot.20250925.0/daml-sdk-3.4.0-snapshot.20250925.0-macos-x86_64.tar.gz => from nix
https://github.com/digital-asset/daml/releases/download/v3.4.0-snapshot.20250925.0/daml-sdk-3.4.0-snapshot.20250919.14213.0.vfa842949-macos-x86_64.tar.gz

To copy daml-ctl to daml finance, run
        cp ../daml-ctl/.daml/dist/daml-ctl-3.99.0.20250926.1.dar .lib 

