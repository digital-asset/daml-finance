# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

Stable Packages
---------------

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 1.0.0              | 1.0.1          |
| ContingentClaims.Lifecycle                 | 1.0.0              | 1.0.1          |
| Daml.Finance.Account                       | 1.0.0              | 2.0.0          |
| Daml.Finance.Claims                        | 1.0.0              | 1.0.1          |
| Daml.Finance.Data                          | 1.0.0              | 2.0.0          |
| Daml.Finance.Holding                       | 1.0.1              | 2.0.0          |
| Daml.Finance.Instrument.Generic            | 1.0.0              | 2.0.0          |
| Daml.Finance.Instrument.Token              | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Account             | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Claims              | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Data                | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Holding             | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Base     | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Generic  | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Token    | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Lifecycle           | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Settlement          | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Types.Common        | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Types.Date          | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Util                | 1.0.0              | 2.0.0          |
| Daml.Finance.Lifecycle                     | 1.0.0              | 2.0.0          |
| Daml.Finance.Settlement                    | 1.0.1              | 2.0.0          |
| Daml.Finance.Util                          | 1.0.0              | 2.0.0          |

Early Access Packages
---------------------

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| Daml.Finance.Instrument.Bond               | 0.2.0              | 0.3.0          |
| Daml.Finance.Instrument.Equity             | 0.2.0              | 0.3.0          |
| Daml.Finance.Instrument.Option             |                    | 0.3.0          |
| Daml.Finance.Instrument.Swap               | 0.2.0              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Bond     | 0.2.0              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Equity   | 0.2.0              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Option   |                    | 0.3.0          |
| Daml.Finance.Interface.Instrument.Swap     | 0.2.0              | 0.3.0          |

Experimental
------------
| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                 |                    | 0.2.1          |

## Pending changes

#### ContingentClaims.Core

- SDK version 2.6

#### ContingentClaims.Lifecycle

- SDK version 2.6

#### Daml.Finance.Account

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Claims

- Dependencies update

- SDK version 2.6

#### Daml.Finance.Data

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Holding

- Fix bug in the implementation of `Fungible.Merge`

- Improve error message when acquiring a lock

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Instrument.Bond

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Instrument.Equity

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Instrument.Generic

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Instrument.Option

- Add package

- Add support for cash-settled, auto-exercising European call and put options

- SDK version 2.6

- Requires

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

- SDK version 2.6

- Requires

#### Daml.Finance.Instrument.Token

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Account

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Claims

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Data

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Holding

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Instrument.Base

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Instrument.Bond

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Instrument.Equity

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Instrument.Generic

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Instrument.Option

- Add package

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Instrument.Swap

- Updates to data types related to interest rate compounding and payment lag

- Updates to data types related to Term period

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Instrument.Token

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Lifecycle

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Settlement

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Types.Common

- SDK version 2.6

#### Daml.Finance.Interface.Types.Date

- Introduce `ScheduleFrequency` data type

- Added `NoRollConvention` to `RollConventionEnum`. It applies to `D` and `W` periods

- `PeriodicSchedule.frequency` is of type `ScheduleFrequency` instead of `Frequency`

- SDK version 2.6

- Requires

#### Daml.Finance.Interface.Util

- SDK version 2.6

- Requires (type constraints for requiring to implement `Disclosure`)

#### Daml.Finance.Lifecycle

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Settlement

- Additional sanity checks added to `Instruction`

- Dependencies update

- SDK version 2.6

- Requires

#### Daml.Finance.Util

- `calcPeriodDcf` and `calcPeriodDcfActActISMA` take a `ScheduleFrequency` instead of a `Frequency`

- Dependencies update

- SDK version 2.6

- Requires
