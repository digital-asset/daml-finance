# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

Stable Packages
---------------

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 1.0.0              | 1.0.1          |
| ContingentClaims.Lifecycle                 | 1.0.0              | 1.0.1          |
| Daml.Finance.Account                       | 1.0.1              | 1.0.2          |
| Daml.Finance.Claims                        | 1.0.1              | 1.0.2          |
| Daml.Finance.Data                          | 1.0.1              | 1.0.2          |
| Daml.Finance.Holding                       | 1.0.2              | 1.1.0          |
| Daml.Finance.Instrument.Generic            | 1.0.1              | 1.0.2          |
| Daml.Finance.Instrument.Token              | 1.0.1              | 1.0.2          |
| Daml.Finance.Interface.Account             | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Claims              | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Data                | 2.0.0              | 2.0.1          |
| Daml.Finance.Interface.Holding             | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Instrument.Base     | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Instrument.Generic  | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Instrument.Token    | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Lifecycle           | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Settlement          | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Types.Common        | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Types.Date          | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Util                | 1.0.0              | 1.0.0          |
| Daml.Finance.Lifecycle                     | 1.0.1              | 1.0.2          |
| Daml.Finance.Settlement                    | 1.0.2              | 1.1.0          |
| Daml.Finance.Util                          | 2.0.0              | 2.0.1          |

Early Access Packages
---------------------

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                 | 0.2.0              | 0.2.1          |
| Daml.Finance.Instrument.Bond               | 0.2.1              | 0.2.2          |
| Daml.Finance.Instrument.Equity             | 0.2.1              | 0.2.2          |
| Daml.Finance.Instrument.Option             | 0.1.0              | 0.1.1          |
| Daml.Finance.Instrument.Swap               | 0.2.1              | 0.2.2          |
| Daml.Finance.Interface.Instrument.Bond     | 0.2.1              | 0.2.1          |
| Daml.Finance.Interface.Instrument.Equity   | 0.2.0              | 0.2.1          |
| Daml.Finance.Interface.Instrument.Option   | 0.1.0              | 0.1.0          |
| Daml.Finance.Interface.Instrument.Swap     | 0.2.1              | 0.2.2          |

## Pending changes

#### ContingentClaims.Core

- Style changes

#### ContingentClaims.Lifecycle

- Dependencies update

- Style changes

#### ContingentClaims.Valuation

- Dependencies update

- Style changes

#### Daml.Finance.Account

- Dependencies update

#### Daml.Finance.Claims

- Dependencies update

- Style changes

#### Daml.Finance.Data

- Dependencies update

- Style changes

#### Daml.Finance.Holding

- Dependencies update

- Added default `splitImpl` and `mergeImpl` for `Fungible` to `Util.daml` (also generalized the
  `acquireImpl` and `releaseImpl` to not rely on an attribute called "lock").

#### Daml.Finance.Instrument.Bond

- Dependencies update

- Style changes

#### Daml.Finance.Instrument.Equity

- Dependencies update

#### Daml.Finance.Instrument.Generic

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

#### Daml.Finance.Instrument.Option

- Dependencies update

- Style changes

- Add physically settled European options (EuropeanPhysical).

- Renamed cash-settled European options (European -> EuropeanCash).

- Add dividend options

#### Daml.Finance.Instrument.Swap

- Dependencies update

- Style changes

#### Daml.Finance.Instrument.Token

- Dependencies update

#### Daml.Finance.Interface.Account

- Dependencies update

#### Daml.Finance.Interface.Claims

- Dependencies update

#### Daml.Finance.Interface.Data

- Dependencies update

- Style changes

#### Daml.Finance.Interface.Holding

- Style changes

#### Daml.Finance.Interface.Instrument.Base

- Dependencies update

- Style changes

#### Daml.Finance.Interface.Instrument.Bond

- Dependencies update

- Added `GetView` to all instruments

#### Daml.Finance.Interface.Instrument.Equity

- Dependencies update

#### Daml.Finance.Interface.Instrument.Generic

- Dependencies update

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

#### Daml.Finance.Interface.Instrument.Option

- Dependencies update

- Add physically settled European options (EuropeanPhysical).

- Renamed cash-settled European options (European -> EuropeanCash).

- Added `GetView` to all instruments

- Add dividend options

#### Daml.Finance.Interface.Instrument.Swap

- Dependencies update

- Style changes

- Added `GetView` to all instruments

#### Daml.Finance.Interface.Instrument.Token

- Dependencies update

#### Daml.Finance.Interface.Lifecycle

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

#### Daml.Finance.Interface.Settlement

- Style changes

#### Daml.Finance.Interface.Types.Common

#### Daml.Finance.Interface.Types.Date

#### Daml.Finance.Interface.Util

#### Daml.Finance.Lifecycle

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

#### Daml.Finance.Settlement

- Dependencies update

- Style changes

- Added locking to the `Instruction`

#### Daml.Finance.Util

- Style changes
