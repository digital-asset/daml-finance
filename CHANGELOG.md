# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

Stable Packages
---------------

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 1.0.0              | 1.0.0          |
| ContingentClaims.Lifecycle                 | 1.0.0              | 1.0.0          |
| Daml.Finance.Account                       | 1.0.0              | 1.0.1          |
| Daml.Finance.Claims                        | 1.0.0              | 1.0.1          |
| Daml.Finance.Data                          | 1.0.0              | 1.0.1          |
| Daml.Finance.Holding                       | 1.0.0              | 1.0.1          |
| Daml.Finance.Instrument.Generic            | 1.0.0              | 1.0.1          |
| Daml.Finance.Instrument.Token              | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Account             | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Claims              | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Data                | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Holding             | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Instrument.Base     | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Instrument.Generic  | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Instrument.Token    | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Lifecycle           | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Settlement          | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Types.Common        | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Types.Date          | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Util                | 1.0.0              | 1.0.0          |
| Daml.Finance.Lifecycle                     | 1.0.0              | 1.0.1          |
| Daml.Finance.Settlement                    | 1.0.0              | 1.0.1          |
| Daml.Finance.Util                          | 1.0.0              | 2.0.0          |

Early Access Packages
---------------------

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| Daml.Finance.Instrument.Bond               | 0.2.0              | 0.2.1          |
| Daml.Finance.Instrument.Equity             | 0.2.0              | 0.2.1          |
| Daml.Finance.Instrument.Option             |                    | 0.1.0          |
| Daml.Finance.Instrument.Swap               | 0.2.0              | 0.2.1          |
| Daml.Finance.Interface.Instrument.Bond     | 0.2.0              | 0.2.1          |
| Daml.Finance.Interface.Instrument.Equity   | 0.2.0              | 0.2.0          |
| Daml.Finance.Interface.Instrument.Option   |                    | 0.1.0          |
| Daml.Finance.Interface.Instrument.Swap     | 0.2.0              | 0.2.1          |

## Pending changes

#### ContingentClaims.Core

#### ContingentClaims.Lifecycle

#### Daml.Finance.Account

- Dependencies update

#### Daml.Finance.Claims

- Dependencies update

#### Daml.Finance.Data

- Dependencies update

#### Daml.Finance.Holding

- Fix bug in the implementation of `Fungible.Merge`

- Improve error message when acquiring a lock

- Dependencies update

#### Daml.Finance.Instrument.Bond

- Dependencies update

#### Daml.Finance.Instrument.Equity

#### Daml.Finance.Instrument.Generic

- Dependencies update

#### Daml.Finance.Instrument.Option

- Add package

- Add support for cash-settled, auto-exercising European call and put options

#### Daml.Finance.Instrument.Swap

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

#### Daml.Finance.Instrument.Token

- Dependencies update

#### Daml.Finance.Interface.Account

#### Daml.Finance.Interface.Claims

#### Daml.Finance.Interface.Data

- Dependencies update

#### Daml.Finance.Interface.Holding

#### Daml.Finance.Interface.Instrument.Base

#### Daml.Finance.Interface.Instrument.Bond

- Dependencies update

#### Daml.Finance.Interface.Instrument.Equity

#### Daml.Finance.Interface.Instrument.Generic

#### Daml.Finance.Interface.Instrument.Option

#### Daml.Finance.Interface.Instrument.Swap

- Updates to data types related to interest rate compounding and payment lag

- Updates to data types related to Term period

#### Daml.Finance.Interface.Instrument.Token

#### Daml.Finance.Interface.Lifecycle

#### Daml.Finance.Interface.Settlement

#### Daml.Finance.Interface.Types.Common

#### Daml.Finance.Interface.Types.Date

- Introduce `ScheduleFrequency` data type

- Added `NoRollConvention` to `RollConventionEnum`. It applies to `D` and `W` periods

- `PeriodicSchedule.frequency` is of type `ScheduleFrequency` instead of `Frequency`

#### Daml.Finance.Interface.Util

#### Daml.Finance.Lifecycle

- Dependencies update

#### Daml.Finance.Settlement

- Additional sanity checks added to `Instruction`

- Dependencies update

#### Daml.Finance.Util

- `calcPeriodDcf` and `calcPeriodDcfActActISMA` take a `ScheduleFrequency` instead of a `Frequency`

- Dependencies update
