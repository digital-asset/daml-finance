.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Interface.Instrument.Token.V4
==========================================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Interface.Instrument.Token
=======================================

Version 3.0.0
*************

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- The `Remove` choice was removed from the `Factory` (it is newly part of the `Base` instrument
  interface).

- Renamed the `F` type synonym to `I`.

Version 2.0.0
*************

- Update of SDK version and dependencies.

- The `Create` choice on the instrument factory returns the corresponding interface (rather than the
  base instrument interface).

- Make use of the `requires` keyword to enforce the interface hierarchy (in the particular
  `asDisclosure` and `asBaseInstrument` implementations were removed).
