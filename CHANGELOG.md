# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

## Stable Packages

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 2.0.0              | 2.0.1          |
| ContingentClaims.Lifecycle                 | 2.0.0              | 2.0.1          |
| Daml.Finance.Account                       | 2.0.0              | 3.0.0          |
| Daml.Finance.Claims                        | 2.0.0              | 2.1.0          |
| Daml.Finance.Data                          | 2.0.0              | 3.0.0          |
| Daml.Finance.Holding                       | 2.0.0              | 3.0.0          |
| Daml.Finance.Instrument.Bond               | 1.0.0              | 2.0.0          |
| Daml.Finance.Instrument.Generic            | 2.0.0              | 3.0.0          |
| Daml.Finance.Instrument.Token              | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Account             | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Claims              | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Data                | 3.0.0              | 3.1.0          |
| Daml.Finance.Interface.Holding             | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Instrument.Base     | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Instrument.Bond     | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Generic  | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Instrument.Token    | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Instrument.Types    |                    | 1.0.0          |
| Daml.Finance.Interface.Lifecycle           | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Settlement          | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Types.Common        | 1.0.1              | 2.0.0          |
| Daml.Finance.Interface.Types.Date          | 2.0.1              | 2.1.0          |
| Daml.Finance.Interface.Util                | 2.0.0              | 2.1.0          |
| Daml.Finance.Lifecycle                     | 2.0.0              | 3.0.0          |
| Daml.Finance.Settlement                    | 2.0.0              | 3.0.0          |
| Daml.Finance.Util                          | 3.0.0              | 3.1.0          |

## Early Access Packages

| Package                                             | Released version   | Target version |
|-----------------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                          | 0.2.1              | 0.2.2          |
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

- Update of SDK version and dependencies.

### ContingentClaims.Lifecycle

- Update of SDK version and dependencies.

### ContingentClaims.Valuation

- Update of SDK version and dependencies.

### Daml.Finance.Account

- Update of SDK version and dependencies.

- The Account now utilizes a key (specifically, a `HoldingFactoryKey`), instead of a `ContractId`,
  to reference its `Daml.Finance.Interface.Holding.Factory`.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Account`
  interface).

- The `Account` has been enhanced to implement the `Lockable` interface with the `custodian`
  as requierd authorizer for executing the `Acquire` choice (an alternative implementation that does
  not utilize `Lockable` remains viable).

### Daml.Finance.Claims

- Update of SDK version and dependencies.

- Added builder function for SOFR style rates (via a compounded index).

### Daml.Finance.Data

- Update of SDK version and dependencies.

- Removed the key from the `LedgerTime` as it was unused.

### Daml.Finance.Holding

- Update of SDK version and dependencies.

- Added an `id : Id` field to the `Factory`.

- Replaced all factories by a single `Factory` for all holding implementations.

- Added an assert that the `custodian` remains the same for both the sending and receiving accounts.

- Removed the consistence check that credited and debited holdings must have the same
  `templateTypeRep`, newly they need to have the same `HoldingStandard` (but can have different
  implementations).

- Added a `Fungible` implementation, and renamed the `NonTransferable` and `Fungible`
  implementations to `BaseHolding` and `TransferableFungible`, respectively.

- The holding implementations newly `ensure` that its desired `HoldingStandard` is met.

- The locking logic was factored out to a separate `Lockable` interface (within the
  `Daml.Finance.Interface.Util` package), and the `acquireImpl` and `releaseImpl` utility functions
  moved to the `Lockable` module in the `Daml.Finance.Util` implementation package.

- The `Transfer`, `Split`, `Merge`, and `Debit` actions on holdings are prohibited in a locked
  state, requiring them to be unlocked first. Notably, the type signatures for `splitImpl` and
  `mergeImpl` have been modified, and the re-entrant lock logic of `transferImpl` removed.

- Added `T` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Instrument.Bond

- Update of SDK version and dependencies.

- Added a `HoldingStandard` field to the implementation.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Base`
  instrument interface).

- Added `T` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

- Added support for SOFR style rates (via a compounded index) to the floating rate bond.

### Daml.Finance.Instrument.Equity

- Update of SDK version and dependencies.

- Added a `HoldingStandard` field to the implementation.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Base`
  instrument interface).

- Added `T` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Instrument.Generic

- Update of SDK version and dependencies.

- Added a `HoldingStandard` field to the implementation.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Base`
  instrument interface).

- Added `T` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Instrument.Option

- Update of SDK version and dependencies.

- Added a `HoldingStandard` field to the implementation.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Base`
  instrument interface).

- Added `T` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

- Removed the `Remove` choice from the election factory.

### Daml.Finance.Instrument.StructuredProduct

- First release.

### Daml.Finance.Instrument.Swap

- Update of SDK version and dependencies.

- Added a `HoldingStandard` field to the implementation.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Base`
  instrument interface).

- Added `T` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

- Added support for SOFR style rates (via a compounded index) to the interest rate swap.

### Daml.Finance.Instrument.Token

- Update of SDK version and dependencies.

- Added a `HoldingStandard` field to the implementation.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Base`
  instrument interface).

- Added `T` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Interface.Account

- Update of SDK version and dependencies.

- Removed the `ContractId Holding.Factory` from the account view.

- The `Remove` choice, which was previously a choice of the `Factory`, has now been reassigned to
  the `Account`.

- The `Create` choice of the account's `Factory` has been adapted, it now takes a
  `HoldingFactoryKey` instead of the `ContractId Daml.Finance.Interface.Holding.Factory` as input

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Interface.Claims

- Update of SDK version and dependencies.

- Add general support to replay previous events of different types (not only elections).

### Daml.Finance.Interface.Data

- Update of SDK version and dependencies.

- Added `I` as type synonym for each `Factory` in the package (the `F` type synonyms are to be
  deprecated).

### Daml.Finance.Interface.Holding

- Update of SDK version and dependencies.

- Factored out the locking logic from the base `Holding` interface to a separate interface called
  `Lockable` of the `Daml.Finance.Interface.Util` package.

- Updated the `Daml.Finance.Interface.Holding.Factory` to use a key, employing a `Reference`
  template and the `HoldingFactoryKey` data type. Additionally, It also requires the `Disclosure.I`
  and has a `getKey` method and `Remove` choice.

- Removed the requirement that a `Fungible.I` requires `Transferable.I`.

- Renamed `Base` to `Holding`.

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Interface.Instrument.Base

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- Moved the `Remove` choice from the `Factory` to the `Instrument`.

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

- Made the `issuer` a single-maintainer of the `Instrument` key.

### Daml.Finance.Interface.Instrument.Bond

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- The `Remove` choice was removed from the `Factory` (it is newly part of the `Base` instrument
  interface).

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

- Added support for SOFR style rates (via a compounded index) to the floating rate bond.

### Daml.Finance.Interface.Instrument.Equity

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- The `Remove` choice was removed from the `Factory` (it is newly part of the `Base` instrument
  interface).

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Interface.Instrument.Generic

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- The `Remove` choice was removed from the `Factory` (it is newly part of the `Base` instrument
  interface).

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Interface.Instrument.Option

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- Removed the `Remove` choice from the option dividend election `Factory`.

- The `Remove` choice was removed from the `Factory` (it is newly part of the `Base` instrument
  interface).

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Interface.Instrument.StructuredProduct

- First release.

### Daml.Finance.Interface.Instrument.Swap

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- The `Remove` choice was removed from the `Factory` (it is newly part of the `Base` instrument
  interface).

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

- Added support for SOFR style rates (via a compounded index) to the interest rate swap.

### Daml.Finance.Interface.Instrument.Token

- Update of SDK version and dependencies.

- The `InstrumentKey` is extended by the `HoldingStandard` field.

- The `Remove` choice was removed from the `Factory` (it is newly part of the `Base` instrument
  interface).

- Added `I` as type synonym for `Factory` (the `F` type synonym is to be deprecated).

### Daml.Finance.Interface.Instrument.Types

- First release.

### Daml.Finance.Interface.Lifecycle

- Update of SDK version and dependencies.

- Added `I` as type synonym for each `Factory` in the package (the `F` type synonyms are to be
  deprecated).

- Changed the `Calculate` choice of the `Effect.I` to take a quantity as argument instead of a
  `ContractId Holding` (in order to not leak information about the holding to the effect provider).

### Daml.Finance.Interface.Settlement

- Update of SDK version and dependencies.

- The `requestors : Parties` was split up into a single-maintainer for the key `instructor : Party`
  and additional signatories `consenters : Parties`. The `Batch` and `Instruction` views were
  amended accordingly.

### Daml.Finance.Interface.Types.Common

- Update of SDK version and dependencies.

- Added a `HoldingFactoryKey` data type used to key holding factories.

- Added a `HoldingStandard` enumeration data type for referring to various holding standards, it
  is newly part of the `InstrumentKey`.

- The `requestors : Parties` field of the `InstrumentKey` was replaced by `instructor : Party` (in
  order to get a single-maintainer of the `Instruction` key).

### Daml.Finance.Interface.Types.Date

- Update of SDK version and dependencies.

- Added new day-count conventions: Act365NL, Basis30365 and Basis30E2360.

### Daml.Finance.Interface.Util

- Update of SDK version and dependencies.

- Added a `Lockable` module containing the interface for locking (the `Acquire` and `Release`
  choices used to be part of the base `Holding` interface).

- Created a module `InterfaceKey` with utility functions for keyed interfaces.

### Daml.Finance.Lifecycle

- Update of SDK version and dependencies.

- The `Calculate` choice of the `Effect` and `ElectionEffect` now takes a quantity as argument
  to reflect the change in the `Effect.I` interface. The implementation of the `ClaimEffect` choice
  body of `Daml.Finance.Lifecycle.Rule.Claim` also changed accordingly.

- Replaced `lookupByKey` by an `exerciseByKey` in the `Distribution` and `Replacement` rule.

- Replaced `providers : Parties` with `provider : Party` in the `Claim` rule (i.e., in the
implementation only).

### Daml.Finance.Settlement

- Update of SDK version and dependencies.

- Removed the check for consistent settled holdings. Now, holdings of the same instrument don't
  need identical `templateTypeRep`, instead they should share the same token standard
  (implementation variations are allowed).

- Splitted up the `requestors : Parties` into a single-maintainer for the key `instructor : Party`
  and additional signatories `consenters : Parties`. The `Batch` and `Instruction` templates were
  amended accordingly.

### Daml.Finance.Util

- Update of SDK version and dependencies.

- Added a `Lockable` module containing the `aquireImpl` and `releaseImpl` locking utitlity
  functions.

- Fix a bug in the schedule roll-out logic.

- Added new day-count conventions: Act365NL, Basis30365 and Basis30E2360.
