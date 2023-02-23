.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Instrument.Swap - Changelog
########################################

Version 0.2.1
*************

- Implement interest rate compounding (several calculation periods per payment period)

- Support a more generic way of specifying notional step schedules

- Support specification of a payment lag

- Efficient calculation of SOFR-like daily compounded reference rates

- Implement arrears reset

- Implement step-up coupon

- Add support for initial stub period that starts before the issue date of the swap

- Improve handling of principal exchange

- Add support for Term period of a swap leg

- Additional improvements required to make the official FpML trades 1..7 work as expected

