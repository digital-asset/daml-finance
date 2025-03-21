.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Interface.Instrument.Option.V0
===========================================

Version 1.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Interface.Instrument.Option.V0
===========================================

Version 0.3.0
*************

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- Removed the `Remove` choice from the option dividend election `Factory`.

- The `Remove` choice was removed from the `Factory` (it is newly part of the `Base` instrument
  interface).

- Renamed the `F` type synonym to `I`.

Version 0.2.0
*************

- Update of SDK version and dependencies.

- The `Create` choice on the instrument factories returns the corresponding interface (rather than
  the base instrument interface).

- Add instruments physically-settled European options, dividend options, barrier options.

- Renamed cash-settled European options to `EuropeanCash`.

- Added `GetView` choice to all instrument interfaces.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure`, `asBaseInstrument`, and `asEvent` methods were removed).
