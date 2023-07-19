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

| Package                                             | Released version   | Target version |
|-----------------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                          | 0.2.0              | 0.2.1          |
| Daml.Finance.Instrument.Bond                        | 0.2.1              | 0.3.0          |
| Daml.Finance.Instrument.Equity                      | 0.2.1              | 0.3.0          |
| Daml.Finance.Instrument.Option                      | 0.1.0              | 0.2.0          |
| Daml.Finance.Instrument.StructuredProduct           |                    | 0.1.0          |
| Daml.Finance.Instrument.Swap                        | 0.2.1              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Bond              | 0.2.1              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Equity            | 0.2.0              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Option            | 0.1.0              | 0.2.0          |
| Daml.Finance.Interface.Instrument.StructuredProduct |                    | 0.1.0          |
| Daml.Finance.Interface.Instrument.Swap              | 0.2.1              | 0.3.0          |

## Pending changes

### ContingentClaims.Core

- Dependencies update

- Style changes

- Add smart constructors: orList & andList

- Add `ObserveAt` observation builder

### ContingentClaims.Lifecycle

- Dependencies update

- Style changes

### ContingentClaims.Valuation

- Dependencies update

- Style changes

### Daml.Finance.Account

- Let the `Account` implement the `Lockable` interface with `custodian` as required authorizer (for
  exercising the `Acquire` choice).

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`
  implementation was removed as well as redundant `HasImplementation` instances)

- Uses `ensure` to ensure that the set of outgoing controllers is non-empty.

- Adds a check that locked holdings can't be debited.

### Daml.Finance.Claims

- Dependencies update

- Style changes

- Add support for hybrid (election & time-based) instruments

- Create version consisting of more than hash of remaining claims: it now includes
  `lastEventTimestamp` as well.

- Create instruments even if they have a Zero claim

### Daml.Finance.Data

- Unecessary `Remove` choice (implementations) were removed.

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asNumericObservable`, `asTimeObservable`, and `asEvent` implementations were removed as well as
  redundant `HasImplementation` instances)

- Removed `key` from `DateClock`.

### Daml.Finance.Holding

- Unecessary `Remove` choice (implementations) were removed.

- As the locking logic from the `Holding.Base` interface was factored out to a separate interface
  called `Lockable` of the `Daml.Finance.Interface.Util` package, the `acquireImpl` and
  `releaseImpl` moved to the `Lockable` module in the `Daml.Finance.Util` implementation package.

- Dependencies update

- Added default `splitImpl` and `mergeImpl` for `Fungible` to `Util.daml` (also generalized the
  `acquireImpl` and `releaseImpl` to not rely on an attribute called "lock")

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBase`, and `asTransferable` implementations were removed as well as redundant
  `HasImplementation` instances)

- Added the new owner as observer of the `Transfer` choice of the `Transferable` interface.

- Fix for locking (don't allow an empty `lockers` set).

- Prohibits the `Transfer`, `Split`, `Merge`, and `Debit` actions on holdings that are in a locked
  state, requiring them to be unlocked first. Adjustments have been made in the corresponding
  implementations to accommodate this change. Notably, the type signatures for `splitImpl` and
  `mergeImpl` have been modified. For `transferImpl`, the re-entrant lock logic has been extracted
  and is now supplied as an independent template in `Daml.Finance.Holding.Test.Transfer`.

### Daml.Finance.Instrument.Bond

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

- Add callable bond instrument

- Make notional configurable on the instrument

### Daml.Finance.Instrument.Equity

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

- Rename `DeclareDividend` to `DeclareDistribution`

### Daml.Finance.Instrument.Generic

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBaseInstrument`, and `asClaim` implementations were removed as well as redundant
  `HasImplementation` instances)

- During lifeycling: create a new instrument also in case of a Zero claim (breaking change).

### Daml.Finance.Instrument.Option

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Style changes

- Add physically settled European options (EuropeanPhysical)

- Renamed cash-settled European options (European -> EuropeanCash)

- Add dividend options

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

- Add barrier options

### Daml.Finance.Instrument.StructuredProduct

- The factory create choices return the corresponding interface (instead of the base interface).

- Add Barrier Reverse Convertible instrument

### Daml.Finance.Instrument.Swap

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

- Refactor using `ObserveAt`

### Daml.Finance.Instrument.Token

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in the particular `asDisclosure` and
  `asBaseInstrument` implementations were removed as well as redundant `HasImplementation`
  instances)

### Daml.Finance.Interface.Account

- Let the `Account` require the `Lockable` interface, effectively allowing to freeze an account.

- Dependencies update

- Removed `type K = AccountKey`

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`
  method was removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Claims

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asBaseInstrument`
  method was removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Data

- Unecessary `Remove` choices were removed from factories.

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asNumericObservable`, and `asTimeObservable` methods were removed as well as the redundant
  `HasImplementation` type class)

### Daml.Finance.Interface.Holding

- Unecessary `Remove` choices were removed from factories.

- Removed unnecessary `ArchiveFungible` choice

- Factored out the locking logic from the `Holding.Base` interface to a separate interface called
  `Lockable` of the `Daml.Finance.Interface.Util` package.

- Dependencies update

- Style changes

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBase`, and `asTransferable` methods were removed as well as the redundant `HasImplementation`
  type class)

- Fix to signature of `disclose` (removed the `actor` argument).

- The `lockers` have been removed as controllers of the `Transfer`, `Split`, and `Merge` choices.
  Any `Holding` in a locked state must first be unlocked before any modifications can be made to it.

### Daml.Finance.Interface.Instrument.Base

- Dependencies update

- Style changes

- Removed `type K = InstrumentKey`

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`
  method was removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Instrument.Bond

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Added `GetView` to all instruments

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed as well as the redundant `HasImplementation` type
  classes)

- Add callable bond instrument

- Make notional configurable on the instrument

### Daml.Finance.Interface.Instrument.Equity

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed as well as the redundant `HasImplementation` type class)

- Rename `DeclareDividend` to `DeclareDistribution`

### Daml.Finance.Interface.Instrument.Generic

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBaseInstrument`, and `asClaim` methods were removed as well as the redundant
  `HasImplementation` type class)

### Daml.Finance.Interface.Instrument.Option

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Add physically settled European options (EuropeanPhysical)

- Renamed cash-settled European options (European -> EuropeanCash)

- Added `GetView` to all instruments

- Add dividend options

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBaseInstrument`, and `asEvent` methods were removed as well as the redundant
  `HasImplementation` type class)

- Add barrier options

### Daml.Finance.Interface.Instrument.StructuredProduct

- The factory create choices return the corresponding interface (instead of the base interface).

- Add Barrier Reverse Convertible instrument

### Daml.Finance.Interface.Instrument.Swap

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Style changes

- Added `GetView` to all instruments

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Instrument.Token

- The factory create choices return the corresponding interface (instead of the base interface).

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed as well as the redundant `HasImplementation` type class)

### Daml.Finance.Interface.Lifecycle

- Changed the `Calculate` choice of the `Effect.I` to take a quantity as argument instead of a
  `ContractId Holding` (in order to not leak information about the holding to the effect provider).

- Unecessary (as of SDK 2.6) `Remove` choices were removed from factories.

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

- Removed `mapWithIndex` as it only was used once, and applied the index in a reverse order.

- Added a `Lockable` module containing the interface for locking (the `Acquire` and `Release`
  choices used to be part of the `Holding.Base` interface).

- Dependencies update

- Makes use of `requires` to enforce the interface hierarchy (in particular the redundant
  `HasImplementation` type class was removed)

### Daml.Finance.Lifecycle

- The `Calculate` choice of the `Effect` and `ElectionEffect` now takes a quantity as argument
  to reflect the change in the `Effect.I` interface. The implementation of the `ClaimEffect` choice
  body of `Daml.Finance.Lifecycle.Rule.Claim` also changed accordingly.

- Unecessary `Remove` choice (implementations) were removed.

- Dependencies update

- Style changes

- Move Election logic from Generic to Lifecycle (to facilitate code reuse)

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asEvent` implementations were removed as well as redundant `HasImplementation` instances)

- Let `Election` and `ElectionEffect` implement the `Disclosure` interface.

- Add check that instruments exist for `Distribution` and `Replacement`

### Daml.Finance.Settlement

- In the settlement `Factory`, the id values used for the `Instruction`s were modified to accurately reflect their order within the `Batch`.

- Fix for `Instruction` and `IntermediatedStatic`, replaced `groupOn` by `sortAndGroupOn`.

- Dependencies update

- Style changes

- Added locking to the `Instruction` (pledge is locked to requestors and the outgoing controllers of
  the sending account)

- Makes use of `requires` to enforce the interface hierarchy (in particular the `asDisclosure`
  implementation was removed as well as redundant `HasImplementation` instances)

- When an `Allocation` (`Approval`) takes place, a check has been added to ensure that either the
  `sender` or the `custodian` (`receiver` or the `custodian`) is among the authorizers.

- When reallocation (re-approval) occurs, it is required that the `signedSenders`
  (`signedReceivers`) of the `Instruction` are part of the authorizing set.

- We have improved the pass-through allocation/approval process by adding additional checks. These
  checks detect settlement failures during the allocation/approval stage, rather than waiting until
  settlement occurs. Specifically, we now verify that the specified pass-through `Instruction` is
  actually part of the `Batch`.

- Removed the `key` from the `Batch` implementation.

### Daml.Finance.Util

- Added test for `sortAndGroupOn`.

- Removed the custom `groupBy` as it was not being used anywhere.

- Added a `Lockable` module containing the `aquireImpl` and `releaseImpl` locking utitlity
  functions.

- Dependencies update

- Style changes
