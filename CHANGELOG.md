# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

## Stable Packages

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 2.0.0              | unchanged      |
| ContingentClaims.Lifecycle                 | 2.0.0              | unchanged      |
| Daml.Finance.Account                       | 2.0.0              | 3.0.0          |
| Daml.Finance.Claims                        | 2.0.0              | 2.1.0          |
| Daml.Finance.Data                          | 2.0.0              | 2.0.1          |
| Daml.Finance.Holding                       | 2.0.0              | 3.0.0          |
| Daml.Finance.Instrument.Bond               | 1.0.0              | 2.0.0          |
| Daml.Finance.Instrument.Generic            | 2.0.0              | 3.0.0          |
| Daml.Finance.Instrument.Token              | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Account             | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Claims              | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Data                | 3.0.0              | 3.0.1          |
| Daml.Finance.Interface.Holding             | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Instrument.Base     | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Instrument.Bond     | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Generic  | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Instrument.Token    | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Instrument.Types    |                    | 1.0.0          |
| Daml.Finance.Interface.Lifecycle           | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Settlement          | 2.0.0              | 2.0.1          |
| Daml.Finance.Interface.Types.Common        | 1.0.1              | 1.1.0          |
| Daml.Finance.Interface.Types.Date          | 2.0.1              | 2.1.0          |
| Daml.Finance.Interface.Util                | 2.0.0              | 2.1.0          |
| Daml.Finance.Lifecycle                     | 2.0.0              | 3.0.0          |
| Daml.Finance.Settlement                    | 2.0.0              | 2.0.1          |
| Daml.Finance.Util                          | 3.0.0              | 3.1.0          |

## Early Access Packages

| Package                                             | Released version   | Target version |
|-----------------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                          | 0.2.1              | unchanged      |
| Daml.Finance.Instrument.Equity                      | 0.3.0              | 0.4.0          |
| Daml.Finance.Instrument.Option                      | 0.2.0              | 0.3.0          |
| Daml.Finance.Instrument.StructuredProduct           |                    | 0.1.0          |
| Daml.Finance.Instrument.Swap                        | 0.3.0              | 0.4.0          |
| Daml.Finance.Interface.Instrument.Equity            | 0.3.0              | 0.4.0          |
| Daml.Finance.Interface.Instrument.Option            | 0.2.0              | 0.3.0          |
| Daml.Finance.Interface.Instrument.StructuredProduct |                    | 0.1.0          |
| Daml.Finance.Interface.Instrument.Swap              | 0.3.0              | 0.4.0          |

## Pending changes

### ContingentClaims.Core

### ContingentClaims.Lifecycle

### ContingentClaims.Valuation

### Daml.Finance.Account

- Dependencies update

- Let the `Account` implement the `Lockable` interface with `custodian` as required authorizer (for
  exercising the `Acquire` choice). Note that `Account.I` is not requiring `Lockable`, so an
  alternative implementation which does not implement `Lockable` is also possible.

- The `Remove` choice of the `Factory` was removed (and is now part of the `Account`).

- Updated the `Account` to reference the `Daml.Finance.Interface.Holding.Factory` using a key
  (of type `HoldingFactoryKey`) rather than the `ContractId Daml.Finance.Interface.Holding.Factory`.

### Daml.Finance.Claims

- Dependencies update

- Added builder function for SOFR style rates (via a compounded index).

### Daml.Finance.Data

- Dependencies update

### Daml.Finance.Holding

- Fix for transfer (adding check that the custodian is the same for the sending and receiving
  account).

- Dependencies update

- As the locking logic from the base `Holding` interface was factored out to a separate interface
  called `Lockable` of the `Daml.Finance.Interface.Util` package, the `acquireImpl` and
  `releaseImpl` moved to the `Lockable` module in the `Daml.Finance.Util` implementation package.

- Prohibits the `Transfer`, `Split`, `Merge`, and `Debit` actions on holdings that are in a locked
  state, requiring them to be unlocked first. Adjustments have been made in the corresponding
  implementations to accommodate this change. Notably, the type signatures for `splitImpl` and
  `mergeImpl` have been modified. For `transferImpl`, the re-entrant lock logic has been extracted
  and is now supplied as an independent template in `Daml.Finance.Holding.Test.Transfer`.

- Removed the consistence check that credited and debited holdings must have the same
  `templateTypeRep`. Now they should be of the same token standard but may have different
  implementations. We rely on code vetting that the holding factories are implemented properly.

- Added an `id : Id` field to the `Factory`.

- Replaced all factories by a single `Factory` for all holding implementations.

- Renamed the `NonTransferable` and `Fungible` implementation to `BaseHolding` and
  `TransferableFungible`, respectively. Also added an implementation for a `Fungible`. Added an
  `ensure` clause to make sure the desired `HoldingStandard` is used.

### Daml.Finance.Instrument.Bond

- Dependencies update

- Added support for SOFR style rates (via a compounded index) to the floating rate bond.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Instrument.Equity

- Dependencies update

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Instrument.Generic

- Dependencies update

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Instrument.Option

- Dependencies update

- Removed the `Remove` choice from the option dividend election `Factory`.

- Moved the `Remove` choice from the factory to the instrument implementation.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Instrument.StructuredProduct

- Dependencies update

- Removed the `Remove` choice of the factory.

- First Release

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Instrument.Swap

- Dependencies update

- Added support for SOFR style rates (via a compounded index) to the interest rate swap.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Instrument.Token

- Dependencies update

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Interface.Account

- Dependencies update

- Removed the `ContractId Holding.F` from the account view.

- The `Remove` choice of the `Factory` was moved to the `Account`.

- Changed the `Create` choice of the account `Factory` to take a key (of type `HoldingFactoryKey`)
  rather than a `ContractId Daml.Finance.Interface.Holding.Factory`.

### Daml.Finance.Interface.Claims

- Dependencies update

- Add general support to replay previous events of different types (not only elections).

### Daml.Finance.Interface.Data

- Dependencies update

### Daml.Finance.Interface.Holding

- Dependencies update

- Factored out the locking logic from the base `Holding` interface to a separate interface called
  `Lockable` of the `Daml.Finance.Interface.Util` package.

- Updated the `Daml.Finance.Interface.Holding.Factory` to use a key, employing a `Reference`
  template and the `HoldingFactoryKey` data type. Additionally, It also requires the `Disclosure.I` and
  has a `getKey` method and `Remove` choice.

- Removed the requirement that a `Fungible.I` requires `Transferable.I`.

- Renamed `Base` to `Holding`.

### Daml.Finance.Interface.Instrument.Base

- Dependencies update

- Removed the `Remove` choice of the factory.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Interface.Instrument.Bond

- Dependencies update

- Removed the `Remove` choice of the factory.

- Added support for SOFR style rates (via a compounded index) to the floating rate bond.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Interface.Instrument.Equity

- Dependencies update

- Removed the `Remove` choice of the factory.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Interface.Instrument.Generic

- Dependencies update

- Removed the `Remove` choice of the factory.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Interface.Instrument.Option

- Dependencies update

- Removed the `Remove` choice from the option dividend election `Factory`.

- Removed the `Remove` choice of the factory.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Interface.Instrument.StructuredProduct

- First release

- Removed the `Remove` choice of the factory.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Interface.Instrument.Swap

- Dependencies update

- Removed the `Remove` choice of the factory.

- Added support for SOFR style rates (via a compounded index) to the interest rate swap.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Interface.Instrument.Token

- Dependencies update

- Removed the `Remove` choice of the factory.

- Added an enumeration data type `HoldingStandard` to the `InstrumentKey` for referring to various
  holding standards.

### Daml.Finance.Interface.Instrument.Types

- New package

### Daml.Finance.Interface.Lifecycle

- Changed the `Calculate` choice of the `Effect.I` to take a quantity as argument instead of a
  `ContractId Holding` (in order to not leak information about the holding to the effect provider).

### Daml.Finance.Interface.Settlement

- Dependencies update

### Daml.Finance.Interface.Types.Common

- Added a `HoldingFactoryKey` data type which is used to key holding factories.

- Added an enumeration data type `HoldingStandard` for referring to various holding standards. It
  is newly part of the `InstrumentKey`.

### Daml.Finance.Interface.Types.Date

- Added new day-count conventions: Act365NL, Basis30365 and Basis30E2360.

### Daml.Finance.Interface.Util

- Added a `Lockable` module containing the interface for locking (the `Acquire` and `Release`
  choices used to be part of the base `Holding` interface).

- Added the `isInstanceOf` utility function which checks whether an interface instance is
  convertible to another interface or template.

- Created a `InterfaceKey` module with utility functions for keyed interfaces.

### Daml.Finance.Lifecycle

- Dependencies update

- The `Calculate` choice of the `Effect` and `ElectionEffect` now takes a quantity as argument
  to reflect the change in the `Effect.I` interface. The implementation of the `ClaimEffect` choice
  body of `Daml.Finance.Lifecycle.Rule.Claim` also changed accordingly.

### Daml.Finance.Settlement

- Dependencies update

- Removed the check for consistent settled holdings. Now, holdings of the same instrument don't
  need identical `templateTypeRep`. Instead, they should share the same token standard
  (implementation variations are allowed). We rely on code vetting that the holding factories are
  implemented properly.

### Daml.Finance.Util

- Added a `Lockable` module containing the `aquireImpl` and `releaseImpl` locking utitlity
  functions.

- Fix a bug in the schedule roll-out logic

- Added new day-count conventions: Act365NL, Basis30365 and Basis30E2360.
