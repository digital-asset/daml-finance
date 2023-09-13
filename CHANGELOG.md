# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

## Stable Packages

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 2.0.0              | TBD            |
| ContingentClaims.Lifecycle                 | 2.0.0              | TBD            |
| Daml.Finance.Account                       | 2.0.0              | TBD            |
| Daml.Finance.Claims                        | 2.0.0              | TBD            |
| Daml.Finance.Data                          | 2.0.0              | TBD            |
| Daml.Finance.Holding                       | 2.0.0              | TBD            |
| Daml.Finance.Instrument.Bond               | 1.0.0              | TBD            |
| Daml.Finance.Instrument.Generic            | 2.0.0              | TBD            |
| Daml.Finance.Instrument.Token              | 2.0.0              | TBD            |
| Daml.Finance.Interface.Account             | 2.0.0              | TBD            |
| Daml.Finance.Interface.Claims              | 2.0.0              | TBD            |
| Daml.Finance.Interface.Data                | 3.0.0              | TBD            |
| Daml.Finance.Interface.Holding             | 2.0.0              | TBD            |
| Daml.Finance.Interface.Instrument.Base     | 2.0.0              | TBD            |
| Daml.Finance.Interface.Instrument.Bond     | 1.0.0              | TBD            |
| Daml.Finance.Interface.Instrument.Generic  | 2.0.0              | TBD            |
| Daml.Finance.Interface.Instrument.Token    | 2.0.0              | TBD            |
| Daml.Finance.Interface.Instrument.Types    |                    | TBD            |
| Daml.Finance.Interface.Lifecycle           | 2.0.0              | TBD            |
| Daml.Finance.Interface.Settlement          | 2.0.0              | TBD            |
| Daml.Finance.Interface.Types.Common        | 1.0.1              | TBD            |
| Daml.Finance.Interface.Types.Date          | 2.0.1              | TBD            |
| Daml.Finance.Interface.Util                | 2.0.0              | TBD            |
| Daml.Finance.Lifecycle                     | 2.0.0              | TBD            |
| Daml.Finance.Settlement                    | 2.0.0              | TBD            |
| Daml.Finance.Util                          | 3.0.0              | TBD            |

## Early Access Packages

| Package                                             | Released version   | Target version |
|-----------------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                          | 0.2.1              | TBD            |
| Daml.Finance.Instrument.Equity                      | 0.3.0              | TBD            |
| Daml.Finance.Instrument.Option                      | 0.2.0              | TBD            |
| Daml.Finance.Instrument.StructuredProduct           |                    | TBD            |
| Daml.Finance.Instrument.Swap                        | 0.3.0              | TBD            |
| Daml.Finance.Interface.Instrument.Equity            | 0.3.0              | TBD            |
| Daml.Finance.Interface.Instrument.Option            | 0.2.0              | TBD            |
| Daml.Finance.Interface.Instrument.StructuredProduct |                    | TBD            |
| Daml.Finance.Interface.Instrument.Swap              | 0.3.0              | TBD            |

## Pending changes

### ContingentClaims.Core

### ContingentClaims.Lifecycle

### ContingentClaims.Valuation

### Daml.Finance.Account

- Dependencies update

- Let the `Account` implement the `Lockable` interface with `custodian` as required authorizer (for
  exercising the `Acquire` choice).

- The `Remove` choice of the `Factory` was removed (and is now part of the `Account`).

### Daml.Finance.Claims

- Dependencies update

- Added builder function for SOFR style rates (via a compounded index).

### Daml.Finance.Data

- Dependencies update

### Daml.Finance.Holding

- Fix for transfer (adding check that the custodian is the same for the sending and receiving
  account).

- Dependencies update

- As the locking logic from the `Holding.Base` interface was factored out to a separate interface
  called `Lockable` of the `Daml.Finance.Interface.Util` package, the `acquireImpl` and
  `releaseImpl` moved to the `Lockable` module in the `Daml.Finance.Util` implementation package.

- Prohibits the `Transfer`, `Split`, `Merge`, and `Debit` actions on holdings that are in a locked
  state, requiring them to be unlocked first. Adjustments have been made in the corresponding
  implementations to accommodate this change. Notably, the type signatures for `splitImpl` and
  `mergeImpl` have been modified. For `transferImpl`, the re-entrant lock logic has been extracted
  and is now supplied as an independent template in `Daml.Finance.Holding.Test.Transfer`.

- Relaxed the check for a consistent transferred holding. Now, the holdings being credited and
  debited don't need identical `templateTypeRep`. Instead, they should share the same token
  standard: `Fungible`, `NonFungible`, or `NonTransferable` (implementation variations are allowed).

### Daml.Finance.Instrument.Bond

- Dependencies update

- Added support for SOFR style rates (via a compounded index) to the floating rate bond.

### Daml.Finance.Instrument.Equity

- Dependencies update

### Daml.Finance.Instrument.Generic

- Dependencies update

### Daml.Finance.Instrument.Option

- Dependencies update

- Removed the `Remove` choice from the option dividend election `Factory`.

- Moved the `Remove` choice from the factory to the instrument implementation.

### Daml.Finance.Instrument.StructuredProduct

- Dependencies update

- Removed the `Remove` choice of the factory.

- First Release

### Daml.Finance.Instrument.Swap

- Dependencies update

- Added support for SOFR style rates (via a compounded index) to the interest rate swap.

### Daml.Finance.Instrument.Token

- Dependencies update

### Daml.Finance.Interface.Account

- Dependencies update

- Let the `Account` require the `Lockable` interface, effectively allowing to freeze an account.

- Removed the `ContractId Holding.F` from the account view.

- The `Remove` choice of the `Factory` was moved to the `Account`.

### Daml.Finance.Interface.Claims

- Dependencies update

### Daml.Finance.Interface.Data

- Dependencies update

### Daml.Finance.Interface.Holding

- Dependencies update

- Factored out the locking logic from the `Holding.Base` interface to a separate interface called
  `Lockable` of the `Daml.Finance.Interface.Util` package.

- Added an enumeration data type `HoldingStandard` for referring to various holding standards:
  `Fungible`, `NonFungible`, or `NonTransferable`. A utility function `getHoldingStandard` for
  retrieving the holding standard of a holding was also added.

### Daml.Finance.Interface.Instrument.Base

- Dependencies update

- Removed the `Remove` choice of the factory.

### Daml.Finance.Interface.Instrument.Bond

- Dependencies update

- Removed the `Remove` choice of the factory.

- Added support for SOFR style rates (via a compounded index) to the floating rate bond.

### Daml.Finance.Interface.Instrument.Equity

- Dependencies update

- Removed the `Remove` choice of the factory.

### Daml.Finance.Interface.Instrument.Generic

- Dependencies update

- Removed the `Remove` choice of the factory.

### Daml.Finance.Interface.Instrument.Option

- Dependencies update

- Removed the `Remove` choice from the option dividend election `Factory`.

- Removed the `Remove` choice of the factory.

### Daml.Finance.Interface.Instrument.StructuredProduct

- First release

- Removed the `Remove` choice of the factory.

### Daml.Finance.Interface.Instrument.Swap

- Dependencies update

- Removed the `Remove` choice of the factory.

- Added support for SOFR style rates (via a compounded index) to the interest rate swap.

### Daml.Finance.Interface.Instrument.Token

- Dependencies update

- Removed the `Remove` choice of the factory.

### Daml.Finance.Interface.Instrument.Types

- New package

### Daml.Finance.Interface.Lifecycle

- Changed the `Calculate` choice of the `Effect.I` to take a quantity as argument instead of a
  `ContractId Holding` (in order to not leak information about the holding to the effect provider).

### Daml.Finance.Interface.Settlement

- Dependencies update

### Daml.Finance.Interface.Types.Common

### Daml.Finance.Interface.Types.Date

- Added new day-count conventions: Act365NL, Basis30365 and Basis30E2360.

### Daml.Finance.Interface.Util

- Added a `Lockable` module containing the interface for locking (the `Acquire` and `Release`
  choices used to be part of the `Holding.Base` interface).

- Added the `isInstanceOf` utility function which checks whether an interface instance is
  convertible to another interface or template.

### Daml.Finance.Lifecycle

- Dependencies update

- The `Calculate` choice of the `Effect` and `ElectionEffect` now takes a quantity as argument
  to reflect the change in the `Effect.I` interface. The implementation of the `ClaimEffect` choice
  body of `Daml.Finance.Lifecycle.Rule.Claim` also changed accordingly.

### Daml.Finance.Settlement

- Dependencies update

- Relaxed the check for consistent settled holdings. Now, holdings of the same instrument don't
  need identical `templateTypeRep`. Instead, they should share the same token standard: `Fungible`,
  `NonFungible`, or `NonTransferable` (implementation variations are allowed).

### Daml.Finance.Util

- Added a `Lockable` module containing the `aquireImpl` and `releaseImpl` locking utitlity
  functions.

- Fix a bug in the schedule roll-out logic

- Added new day-count conventions: Act365NL, Basis30365 and Basis30E2360.
