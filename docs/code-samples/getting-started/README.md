# Code Samples - Getting Started

This folder contains a getting-started daml project which is being referrenced by the docs.
In order to build the project:

1. First update the version and data-dependecies in the `daml.yaml` file.
2. Update the version and dependent libraries in the `get-dependencies.sh` file.
3. Run `./get-dependencies` from the root folder of the project to download the data dependencies
   from github (alternatively copy-paste the corresponding local dars from `../../../.dars` to
   `./lib/`).
4. Run `daml build`.
