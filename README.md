[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/digital-asset/daml/blob/main/LICENSE)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/digital-asset/daml-finance/tree/main.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/digital-asset/daml-finance/tree/main)

Copyright © 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All Rights Reserved. SPDX-License-Identifier: Apache-2.0

## Requirements

To build this library locally, the following requirements are necessary:

1. [Daml SDK](https://docs.daml.com/getting-started/installation.html) (2.x)
2. `make` (3.x)
3. `yq` (4.x)
4. `jq` (>1.5)

## Building

To build the library:

```script
make
```

This will produce individual DAR files for each package in the `.dars` directory.

## Developing

When developing please adhere to the [style guide](./docs/STYLEGUIDE.md).

## Releasing

To perform a release of this project, please follow the guide [here](./docs/RELEASE.MD).
