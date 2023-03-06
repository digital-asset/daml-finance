# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

Stable Packages
---------------

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 1.0.0              | 1.0.1          |
| ContingentClaims.Lifecycle                 | 1.0.0              | 1.0.1          |
| Daml.Finance.Account                       | 1.0.1              | 2.0.0          |
| Daml.Finance.Claims                        | 1.0.1              | 2.0.0          |
| Daml.Finance.Data                          | 1.0.1              | 2.0.0          |
| Daml.Finance.Holding                       | 1.0.2              | 2.0.0          |
| Daml.Finance.Instrument.Generic            | 1.0.1              | 2.0.0          |
| Daml.Finance.Instrument.Token              | 1.0.1              | 2.0.0          |
| Daml.Finance.Interface.Account             | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Claims              | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Data                | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Holding             | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Base     | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Generic  | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Token    | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Lifecycle           | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Settlement          | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Types.Common        | 1.0.0              | 1.1.0          |
| Daml.Finance.Interface.Types.Date          | 2.0.0              | 2.0.1          |
| Daml.Finance.Interface.Util                | 1.0.0              | 2.0.0          |
| Daml.Finance.Lifecycle                     | 1.0.1              | 2.0.0          |
| Daml.Finance.Settlement                    | 1.0.2              | 2.0.0          |
| Daml.Finance.Util                          | 2.0.0              | 2.1.0          |

Early Access Packages
---------------------

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                 | 0.2.0              | 0.2.1          |
| Daml.Finance.Instrument.Bond               | 0.2.1              | 0.3.0          |
| Daml.Finance.Instrument.Equity             | 0.2.1              | 0.3.0          |
| Daml.Finance.Instrument.Option             | 0.1.0              | 0.2.0          |
| Daml.Finance.Instrument.Swap               | 0.2.1              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Bond     | 0.2.1              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Equity   | 0.2.0              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Option   | 0.1.0              | 0.2.0          |
| Daml.Finance.Interface.Instrument.Swap     | 0.2.1              | 0.3.0          |

## Pending changes

#### ContingentClaims.Core

- Dependencies update

- Style changes

#### ContingentClaims.Lifecycle

- Dependencies update

- Style changes

#### ContingentClaims.Valuation

- Dependencies update

- Style changes

#### Daml.Finance.Account

- Dependencies update

- Requires

- Key Table

#### Daml.Finance.Claims

- Dependencies update

- Key Table

- Style changes

#### Daml.Finance.Data

- Dependencies update

- Style changes

- Requires

- Key Table (for `HolidayCalendar` and `TimeObservable`)

- Made use of `TimeObservableKey`

- Moved `HolidayCalendarKey` to `Daml.Finance.Interface.Data`

#### Daml.Finance.Holding

- Dependencies update

- Added default `splitImpl` and `mergeImpl` for `Fungible` to `Util.daml` (also generalized the
  `acquireImpl` and `releaseImpl` to not rely on an attribute called "lock").

- Requires

- Key Table

#### Daml.Finance.Instrument.Bond

- Dependencies update

- Style changes

- Requires

- Key Table

#### Daml.Finance.Instrument.Equity

- Dependencies update

- Requires

- Key Table

#### Daml.Finance.Instrument.Generic

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Requires

- Key Table

#### Daml.Finance.Instrument.Option

- Dependencies update

- Style changes

- Add physically settled European options (EuropeanPhysical).

- Renamed cash-settled European options (European -> EuropeanCash).

- Requires

- Key Table

#### Daml.Finance.Instrument.Swap

- Dependencies update

- Style changes

- Requires

- Key Table

#### Daml.Finance.Instrument.Token

- Dependencies update

- Requires

- Key Table

#### Daml.Finance.Interface.Account

- Dependencies update

- Requires

- Key Table (API change for `AccountFactory.Remove`)

- Removed type synonym for `AccountKey`

#### Daml.Finance.Interface.Claims

- Dependencies update

- Requires

- Key Table (API change for `Claim.GetClaim`)

- Introduced `getHolidayCalendarKeys` method to `Dynamic.Instrument.I`.

#### Daml.Finance.Interface.Data

- Dependencies update

- Style changes

- Requires

- Key Table (for `DateClock`, `HolidayCalendar`, and `LedgerTime`)

- Moved `HolidayCalendarKey` to this package.

#### Daml.Finance.Interface.Holding

- Dependencies update

- Style changes

- Requires

- Key Table (API change for `Transferable.Transfer`)

- Moved the `disclose` and `undisclose` utility functions to
  `Daml.Finance.Interface.Util.Disclosure`

#### Daml.Finance.Interface.Instrument.Base

- Dependencies update

- Style changes

- Requires

- Key Table

- Removed type synonym for `InstrumentKey`

#### Daml.Finance.Interface.Instrument.Bond

- Dependencies update

- Requires

- Key Table (API change for `Factory.Remove`)

#### Daml.Finance.Interface.Instrument.Equity

- Dependencies update

- Requires

- Key Table (API change for `Factory.Remove`)

#### Daml.Finance.Interface.Instrument.Generic

- Dependencies update

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Requires

- Key Table (API change for `Factory.Remove`, `Election.Apply`, and `Election.ApplyElection`)

#### Daml.Finance.Interface.Instrument.Option

- Dependencies update

- Add physically settled European options (EuropeanPhysical).

- Renamed cash-settled European options (European -> EuropeanCash).

- Requires

- Key Table (API change for `Factory.Remove`)

#### Daml.Finance.Interface.Instrument.Swap

- Dependencies update

- Style changes

- Requires

- Key Table (API change for `Factory.Remove`)

#### Daml.Finance.Interface.Instrument.Token

- Dependencies update

- Requires

- Key Table (API change for `Factory.Remove`)

#### Daml.Finance.Interface.Lifecycle

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Requires

- Key Table (added key data type for `TimeObservable` + API change for `Rule.Lifecycle.Evolve`)

#### Daml.Finance.Interface.Settlement

- Dependencies update

- Style changes

- Requires

- Key Table (API change for `Batch.Settle`, `Batch.Cancel`, `Instruction.Allocate`,
  `Instruction.Approve`, `Instruction.Execute`, and `Instruction.Cancel`)

- Added `getKey` interface method to `Instruction` and `Batch` (to be consistent with other
  keyed interfaces).

- Added data type for `BatchKey`.

#### Daml.Finance.Interface.Types.Common

- Dependencies update

- Introduced `TimeObservableKey` data type

- Moved `HolidayCalendarKey` to this package.

#### Daml.Finance.Interface.Types.Date

- Dependencies update

#### Daml.Finance.Interface.Util

- Dependencies update

- Requires (but only type constraint for requiring templates to implement `Disclosure`)

- Key Table (added module `KeyTable` with custom `archive`, `create`, `exercise`, `exerciseByKey`,
  `fetch`, `fetchByKey`, `fetchFromInterface`, and `lookupByKey` for working with explicit key
  tables)

- Utility functions for `disclose` and `undisclose` were moved to this package.

#### Daml.Finance.Lifecycle

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Requires

- Key Table

#### Daml.Finance.Settlement

- Dependencies update

- Style changes

- Requires

- Key Table

#### Daml.Finance.Util

- Dependencies update

- Style changes

- Type signature for utility functions cahnged `setObserversImpl`, `addObserversImpl`, and
  `removeObserversImpl`).
