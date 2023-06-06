# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

## Stable Packages

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 1.0.0              | 1.0.1          |
| ContingentClaims.Lifecycle                 | 1.0.0              | 1.0.1          |
| Daml.Finance.Account                       | 1.0.1              | 2.0.0          |
| Daml.Finance.Claims                        | 1.0.1              | 1.0.2          |
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
| Daml.Finance.Interface.Types.Common        | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Types.Date          | 2.0.0              | 2.0.1          |
| Daml.Finance.Interface.Util                | 1.0.0              | 1.0.1          |
| Daml.Finance.Lifecycle                     | 1.0.1              | 2.0.0          |
| Daml.Finance.Settlement                    | 1.0.2              | 2.0.0          |
| Daml.Finance.Util                          | 2.0.0              | 2.0.1          |

## Early Access Packages

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

### ContingentClaims.Core

- Dependencies update

- Style changes

- Add andList smart constructor

### ContingentClaims.Lifecycle

- Dependencies update

- Style changes

### ContingentClaims.Valuation

- Dependencies update

- Style changes

### Daml.Finance.Account

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`
  implementation was removed as well as redundant `HasImplementation` instances)

### Daml.Finance.Claims

- Dependencies update

- Style changes

### Daml.Finance.Data

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asNumericObservable`, `asTimeObservable`, and `asEvent` implementations were removed as well as
  redundant `HasImplementation` instances)

- Removed `key` from `DateClock`.

### Daml.Finance.Holding

- Dependencies update

- Added default `splitImpl` and `mergeImpl` for `Fungible` to `Util.daml` (also generalized the
  `acquireImpl` and `releaseImpl` to not rely on an attribute called "lock")

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBase`, and `asTransferable` implementations were removed as well as redundant
  `HasImplementation` instances)

### Daml.Finance.Instrument.Bond

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

- Add callable bond instrument

- Make notional configurable on the instrument

### Daml.Finance.Instrument.Equity

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

- Rename `DeclareDividend` to `DeclareDistribution`

### Daml.Finance.Instrument.Generic

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBaseInstrument`, and `asClaim` implementations were removed as well as redundant
  `HasImplementation` instances)

- During lifeycling: create a new instrument also in case of a Zero claim (breaking change).

### Daml.Finance.Instrument.Option

- Dependencies update

- Style changes

- Add physically settled European options (EuropeanPhysical)

- Renamed cash-settled European options (European -> EuropeanCash)

- Add dividend options

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

### Daml.Finance.Instrument.Swap

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

### Daml.Finance.Instrument.Token

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in the particular `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

### Daml.Finance.Interface.Account

- Dependencies update

- Removed `type K = AccountKey`

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`
  method was removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Claims

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asBaseInstrument`
  method was removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Data

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asNumericObservable`, and `asTimeObservable` methods were removed as well as the redundant
  `HasImplementation` type class)

### Daml.Finance.Interface.Holding

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBase`, and `asTransferable` methods were removed as well as the redundant `HasImplementation`
  type class)

- Fix to signature of `disclose` (removed the `actor` argument).

### Daml.Finance.Interface.Instrument.Base

- Dependencies update

- Style changes

- Removed `type K = InstrumentKey`

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`
  method was removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Instrument.Bond

- Dependencies update

- Added `GetView` to all instruments

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed as well as the redundant `HasImplementation` type
  classes)

- Add callable bond instrument

- Make notional configurable on the instrument

### Daml.Finance.Interface.Instrument.Equity

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed as well as the redundant `HasImplementation` type class)

- Rename `DeclareDividend` to `DeclareDistribution`

### Daml.Finance.Interface.Instrument.Generic

- Dependencies update

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBaseInstrument`, and `asClaim` methods were removed as well as the redundant
  `HasImplementation` type class)

### Daml.Finance.Interface.Instrument.Option

- Dependencies update

- Add physically settled European options (EuropeanPhysical)

- Renamed cash-settled European options (European -> EuropeanCash)

- Added `GetView` to all instruments

- Add dividend options

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBaseInstrument`, and `asEvent` methods were removed as well as the redundant
  `HasImplementation` type class)

### Daml.Finance.Interface.Instrument.Swap

- Dependencies update

- Style changes

- Added `GetView` to all instruments

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Instrument.Token

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Lifecycle

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asEvent` methods were removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Settlement

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` was
  removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Types.Common

- Dependencies update

### Daml.Finance.Interface.Types.Date

- Dependencies update

### Daml.Finance.Interface.Util

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the redundant
  `HasImplementation` type class was removed)

### Daml.Finance.Lifecycle

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asEvent` implementations were removed as well as redundant `HasImplementation` instances)

- Add check that instruments exist for `Distribution` and `Replacement`

### Daml.Finance.Settlement

- Dependencies update

- Style changes

- Added locking to the `Instruction` (pledge is locked to requestors and the outgoing controllers of
  the sending account)

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`
  implementation was removed as well as redundant `HasImplementation` instances)

- We have enhanced the pass-through allocation/approval process by incorporating additional checks.
  These checks help to identify settlement failures at the time of allocation/approval itself,
  rather than waiting until settlement occurs. In particular, a check was added that verifies that
  the specified pass-through instruction is part of the batch.

- Removed the `key` from the `Batch` implementation.

### Daml.Finance.Util

- Dependencies update

- Style changes
