[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/digital-asset/daml/blob/main/LICENSE)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/digital-asset/daml-finance/tree/main.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/digital-asset/daml-finance/tree/main)

Copyright © 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All Rights Reserved. SPDX-License-Identifier: Apache-2.0

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

The [Daml Finance Demo App](https://github.com/digital-asset/daml-finance-app/) showcases how
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
make build-all
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

## Important Compatibility Notice (Canton Network)
The `main` branch of this repository currently targets **Daml SDK 3.x** and uses an **unstable Ledger Fragment (LF) version (`2.dev`)**.

As a result:

- Artifacts built from `main` cannot be deployed to the Canton Network / MainNet
- They are not compatible with any production Canton deployment
- This setup is intended for development, experimentation, and forward-looking SDK work only

The use of `LF 2.dev` is required for ongoing development on Daml 3, but is not supported on Canton MainNet, which only accepts stable LF versions.

### What should I use instead?

- For production Canton deployments, use a released version of Daml Finance that targets a stable LF version

This notice will be updated once Daml 3 and its corresponding LF version are fully supported on Canton MainNet.
