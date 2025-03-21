.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Instrument.Bond.V3
===============================

Version 3.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Instrument.Bond
============================

Version 2.0.0
*************

- Update of SDK version and dependencies.

- Added a `HoldingStandard` field to the implementation.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Base`
  instrument interface).

- Renamed the `F` type synonym to `T`.

- Added support for SOFR style rates (via a compounded index) to the floating rate bond.

Version 1.0.0
*************

- Update of SDK version and dependencies.

- The `Create` choice on the instrument factories returns the corresponding interface (rather than
  the base instrument interface).

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure` and `asBaseInstrument` implementations were removed).

- Introduce a new callable bond instrument.

- Add a `notional` field to all instruments.

Version 0.2.1
*************

- Dependencies update.
