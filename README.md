[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/digital-asset/daml/blob/main/LICENSE)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/digital-asset/daml-finance/tree/main.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/digital-asset/daml-finance/tree/main)

Copyright © 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All Rights Reserved. SPDX-License-Identifier: Apache-2.0

# Daml Finance

Welcome to Daml Finance, a collection of purpose-built libraries to enable rapid development of
enterprise-grade tokenization solutions.

## Why do I need it?

Implementing basic financial concepts like ownership or economic terms of an asset is a complex and
tedious task. By providing common building blocks, Daml Finance increases delivery velocity and
shortens the time-to-market when developing Daml applications.

## Getting started using the library

The [Daml Finance documentation](https://digital-asset.github.io/daml-finance/) provides a number of
options to get started using the library.

The [Daml Finance Reference App](https://github.com/digital-asset/daml-finance-app/) showcases how
the library can be integrated into a fully-fledged Daml application.

## Contributing to Daml Finance

### Dependencies

This repo assumes the use of [direnv] for local development, along with a working [Nix]
installation.

[direnv]: https://github.com/direnv/direnv
[Nix]: https://nixos.org/download.html

### Building the library from source

To build the library:

```script
make
```

This will produce individual DAR files for each package in the `.dars` directory.

To test the CI locally, run the following target:

```script
make ci-local
```

To build the documentation follows the steps [here](./docs/README.md).

### Style

When developing please adhere to the [style guide](./STYLEGUIDE.md).

### Releases

To perform a release of this project, please follow the guide [here](./RELEASE.MD).
