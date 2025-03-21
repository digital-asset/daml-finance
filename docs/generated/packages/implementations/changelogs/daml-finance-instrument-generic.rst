.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Instrument.Generic.V4
==================================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Instrument.Generic
===============================

Version 3.0.0
*************

- Update of SDK version and dependencies.

- Added a `HoldingStandard` field to the implementation.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Base`
  instrument interface).

- Renamed the `F` type synonym to `T`.

Version 2.0.0
*************

- Update of SDK version and dependencies.

- The `Create` choice on the instrument factory returns the corresponding interface (rather than the
  base instrument interface).

- Move the `Election` module to the `Lifecycle` package. Also, refactor the `Election` to identify
  the elected sub-tree by a textual tag rather than the actual sub-tree.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure`, `asBaseInstrument`, and `asClaim` implementations were removed).

- A new instrument version is created and returned by the lifecycle rule choice also when the
  instrument expires.

Version 1.0.1
*************

- Dependencies update.
