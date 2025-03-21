.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Instrument.Swap.V0
===============================

Version 1.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Instrument.Swap
============================

Version 0.4.0
*************

- Update of SDK version and dependencies.

- Added a `HoldingStandard` field to the implementation.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Base`
  instrument interface).

- Renamed the `F` type synonym to `T`.

- Added support for SOFR style rates (via a compounded index) to the interest rate swap and the
  asset swap.

- Extended the asset swap to support a basket of underlyings.

- Added a dedicated asset swap `DistributionRule` for total return swaps that pay dividends
  separately from the asset performance.

Version 0.3.0
*************

- Update of SDK version and dependencies.

- The `Create` choice on the instrument factories returns the corresponding interface (rather than
  the base instrument interface).

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure` and `asBaseInstrument` implementations were removed).

- `FpmlSwap` now accepts a non-zero rate fixing lag.

Version 0.2.1
*************

- Implement interest rate compounding (several calculation periods per payment period).

- Support a more generic way of specifying notional step schedules.

- Support specification of a payment lag.

- Efficient calculation of SOFR-like daily compounded reference rates.

- Implement arrears reset.

- Implement step-up coupon.

- Add support for initial stub period that starts before the issue date of the swap.

- Improve handling of principal exchange.

- Add support for Term period of a swap leg.

- Additional improvements required to make the official FpML trades 1..7 work as expected.
