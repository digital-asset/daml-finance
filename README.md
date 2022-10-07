[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/digital-asset/daml/blob/main/LICENSE)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/digital-asset/daml-finance/tree/main.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/digital-asset/daml-finance/tree/main)

Copyright © 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All Rights Reserved. SPDX-License-Identifier: Apache-2.0

## Development

This repo assumes the use of [direnv] for local development, along with a
working [Nix] installation.

[direnv]: https://github.com/direnv/direnv
[Nix]: https://nixos.org/download.html

## Building

To build the library:

```script
make
```

This will produce individual DAR files for each package in the `.dars` directory.

To test CI locally run the following target:

```script
make ci-local
```

To build the documentation follows the steps [here](./docs/README.md).

## Developing

When developing please adhere to the [style guide](./STYLEGUIDE.md).

## Releasing

To perform a release of this project, please follow the guide [here](./RELEASE.MD).
