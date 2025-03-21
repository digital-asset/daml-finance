.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Interface.Instrument.Base.V4
=========================================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Interface.Instrument.Base
======================================

Version 3.0.0
*************

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- Moved the `Remove` choice from the `Factory` to the `Instrument`.

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

- Made the `issuer` a single-maintainer of the `Instrument` key.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

- Renamed the `F` type synonym to `I`.

Version 2.0.0
*************

- Update of SDK version and dependencies.

- Removed type synonym for `InstrumentKey`.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure` method was removed).
