.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Interface.Instrument.Swap.V0
=========================================

Version 1.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Interface.Instrument.Swap
======================================

Version 0.4.0
*************

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- The `Remove` choice was removed from the `Factory` (it is newly part of the `Base` instrument
  interface).

- Renamed the `F` type synonym to `I`.

- Added support for SOFR style rates (via a compounded index) to the interest rate swap and the
  asset swap.

- Extended the asset swap to support a basket of underlyings.

Version 0.3.0
*************

- Update of SDK version and dependencies.

- The `Create` choice on the instrument factories returns the corresponding interface (rather than
  the base instrument interface).

- Added `GetView` choice to all instrument interfaces.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure` and `asBaseInstrument` methods were removed).

Version 0.2.1
*************

- Updates to data types related to interest rate compounding and payment lag.

- Updates to data types related to Term period.
