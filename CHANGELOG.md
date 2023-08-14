# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

## Stable Packages

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 2.0.0              | 2.0.0          |
| ContingentClaims.Lifecycle                 | 2.0.0              | 2.0.0          |
| Daml.Finance.Account                       | 2.0.0              | 2.0.0          |
| Daml.Finance.Claims                        | 2.0.0              | 2.0.0          |
| Daml.Finance.Data                          | 2.0.0              | 2.0.0          |
| Daml.Finance.Holding                       | 2.0.0              | 2.0.0          |
| Daml.Finance.Instrument.Bond               | 1.0.0              | 1.0.0          |
| Daml.Finance.Instrument.Generic            | 2.0.0              | 2.0.0          |
| Daml.Finance.Instrument.Token              | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Account             | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Claims              | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Data                | 3.0.0              | 3.0.0          |
| Daml.Finance.Interface.Holding             | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Base     | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Bond     | 1.0.0              | 1.0.0          |
| Daml.Finance.Interface.Instrument.Generic  | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Token    | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Lifecycle           | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Settlement          | 2.0.0              | 2.0.0          |
| Daml.Finance.Interface.Types.Common        | 1.0.1              | 1.0.1          |
| Daml.Finance.Interface.Types.Date          | 2.0.1              | 2.0.1          |
| Daml.Finance.Interface.Util                | 2.0.0              | 2.0.0          |
| Daml.Finance.Lifecycle                     | 2.0.0              | 2.0.0          |
| Daml.Finance.Settlement                    | 2.0.0              | 2.0.0          |
| Daml.Finance.Util                          | 3.0.0              | 3.0.0          |

## Early Access Packages

| Package                                             | Released version   | Target version |
|-----------------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                          | 0.2.1              | 0.2.1          |
| Daml.Finance.Instrument.Equity                      | 0.3.0              | 0.3.0          |
| Daml.Finance.Instrument.Option                      | 0.2.0              | 0.2.0          |
| Daml.Finance.Instrument.StructuredProduct           |                    | 0.1.0          |
| Daml.Finance.Instrument.Swap                        | 0.3.0              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Equity            | 0.3.0              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Option            | 0.2.0              | 0.2.0          |
| Daml.Finance.Interface.Instrument.StructuredProduct |                    | 0.1.0          |
| Daml.Finance.Interface.Instrument.Swap              | 0.3.0              | 0.3.0          |

## Pending changes

### ContingentClaims.Core

### ContingentClaims.Lifecycle

### ContingentClaims.Valuation

### Daml.Finance.Account

- Dependencies update

- Let the `Account` implement the `Lockable` interface with `custodian` as required authorizer (for
  exercising the `Acquire` choice).

### Daml.Finance.Claims

### Daml.Finance.Data

### Daml.Finance.Holding

- Dependencies update

- As the locking logic from the `Holding.Base` interface was factored out to a separate interface
  called `Lockable` of the `Daml.Finance.Interface.Util` package, the `acquireImpl` and
  `releaseImpl` moved to the `Lockable` module in the `Daml.Finance.Util` implementation package.

- Prohibits the `Transfer`, `Split`, `Merge`, and `Debit` actions on holdings that are in a locked
  state, requiring them to be unlocked first. Adjustments have been made in the corresponding
  implementations to accommodate this change. Notably, the type signatures for `splitImpl` and
  `mergeImpl` have been modified. For `transferImpl`, the re-entrant lock logic has been extracted
  and is now supplied as an independent template in `Daml.Finance.Holding.Test.Transfer`.

### Daml.Finance.Instrument.Bond

### Daml.Finance.Instrument.Equity

### Daml.Finance.Instrument.Generic

### Daml.Finance.Instrument.Option

### Daml.Finance.Instrument.StructuredProduct

### Daml.Finance.Instrument.Swap

### Daml.Finance.Instrument.Token

### Daml.Finance.Interface.Account

- Dependencies update

- Let the `Account` require the `Lockable` interface, effectively allowing to freeze an account.

### Daml.Finance.Interface.Claims

### Daml.Finance.Interface.Data

### Daml.Finance.Interface.Holding

- Factored out the locking logic from the `Holding.Base` interface to a separate interface called
  `Lockable` of the `Daml.Finance.Interface.Util` package.

### Daml.Finance.Interface.Instrument.Base

### Daml.Finance.Interface.Instrument.Bond

### Daml.Finance.Interface.Instrument.Equity

### Daml.Finance.Interface.Instrument.Generic

### Daml.Finance.Interface.Instrument.Option

### Daml.Finance.Interface.Instrument.StructuredProduct

### Daml.Finance.Interface.Instrument.Swap

### Daml.Finance.Interface.Instrument.Token

### Daml.Finance.Interface.Lifecycle

- Changed the `Calculate` choice of the `Effect.I` to take a quantity as argument instead of a
  `ContractId Holding` (in order to not leak information about the holding to the effect provider).

### Daml.Finance.Interface.Settlement

### Daml.Finance.Interface.Types.Common

### Daml.Finance.Interface.Types.Date

### Daml.Finance.Interface.Util

- Added a `Lockable` module containing the interface for locking (the `Acquire` and `Release`
  choices used to be part of the `Holding.Base` interface).

### Daml.Finance.Lifecycle

- The `Calculate` choice of the `Effect` and `ElectionEffect` now takes a quantity as argument
  to reflect the change in the `Effect.I` interface. The implementation of the `ClaimEffect` choice
  body of `Daml.Finance.Lifecycle.Rule.Claim` also changed accordingly.

### Daml.Finance.Settlement

### Daml.Finance.Util

- Added a `Lockable` module containing the `aquireImpl` and `releaseImpl` locking utitlity
  functions.