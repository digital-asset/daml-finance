.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

ContingentClaims.Core.V3
========================

Version 3.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

- Bumps `daml-ctl` to `2.5.0`.

ContingentClaims.Core
=====================

Version 2.0.1
*************

- Update of SDK version and dependencies.

Version 2.0.0
*************

- Update of SDK version and dependencies.

- Add `orList` and `andList` smart constructors.

- Add `ObserveAt` observation builder, used to explicitly state when an `Observation` should be
  observed.

- Refactor `or` and `anytime` smart constructors to identify electable sub-trees by a textual tag.
