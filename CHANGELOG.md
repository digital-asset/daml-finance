# Changelog

This document tracks pending changes to packages. It is facilitating the write-up of release notes.

## Stable Packages

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Core                      | 1.0.0              | 2.0.0          |
| ContingentClaims.Lifecycle                 | 1.0.0              | 2.0.0          |
| Daml.Finance.Account                       | 1.0.1              | 2.0.0          |
| Daml.Finance.Claims                        | 1.0.1              | 2.0.0          |
| Daml.Finance.Data                          | 1.0.1              | 2.0.0          |
| Daml.Finance.Holding                       | 1.0.2              | 2.0.0          |
| Daml.Finance.Instrument.Bond               | 0.2.1              | 1.0.0          |
| Daml.Finance.Instrument.Generic            | 1.0.1              | 2.0.0          |
| Daml.Finance.Instrument.Token              | 1.0.1              | 2.0.0          |
| Daml.Finance.Interface.Account             | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Claims              | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Data                | 2.0.0              | 3.0.0          |
| Daml.Finance.Interface.Holding             | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Base     | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Bond     | 0.2.1              | 1.0.0          |
| Daml.Finance.Interface.Instrument.Generic  | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Instrument.Token    | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Lifecycle           | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Settlement          | 1.0.0              | 2.0.0          |
| Daml.Finance.Interface.Types.Common        | 1.0.0              | 1.0.1          |
| Daml.Finance.Interface.Types.Date          | 2.0.0              | 2.0.1          |
| Daml.Finance.Interface.Util                | 1.0.0              | 2.0.0          |
| Daml.Finance.Lifecycle                     | 1.0.1              | 2.0.0          |
| Daml.Finance.Settlement                    | 1.0.2              | 2.0.0          |
| Daml.Finance.Util                          | 2.0.0              | 3.0.0          |

## Early Access Packages

| Package                                    | Released version   | Target version |
|--------------------------------------------|--------------------|----------------|
| ContingentClaims.Valuation                 | 0.2.0              | 0.2.1          |
| Daml.Finance.Instrument.Equity             | 0.2.1              | 0.3.0          |
| Daml.Finance.Instrument.Option             | 0.1.0              | 0.2.0          |
| Daml.Finance.Instrument.Swap               | 0.2.1              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Equity   | 0.2.0              | 0.3.0          |
| Daml.Finance.Interface.Instrument.Option   | 0.1.0              | 0.2.0          |
| Daml.Finance.Interface.Instrument.Swap     | 0.2.1              | 0.3.0          |

## Pending changes

### ContingentClaims.Core

- Update of SDK version and dependencies

- Add `orList` and `andList` smart constructors

- Add `ObserveAt` observation builder, used to explicitly state when an `Observation` should be observed

- Refactor `or` and `anytime` smart constructors to identify
electable sub-trees by a textual tag

### ContingentClaims.Lifecycle

- Update of SDK version and dependencies

- Refactor `exercise` to identify the elected sub-tree by a textual tag rather than the actual sub-tree

### ContingentClaims.Valuation

- Update of SDK version and dependencies

### Daml.Finance.Account

- Update of SDK version and dependencies

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` implementation was removed)

- Use `ensure` to ensure that the set of outgoing controllers is non-empty

### Daml.Finance.Claims

- Update of SDK version and dependencies

- The lifecycle rule supports a combination of elections and time-based events

- Improve tagging of new instrument versions in the lifecycle rule

- A new instrument version is created and returned by the `Evolve` choice also when the instrument expires

### Daml.Finance.Data

- Update of SDK version and dependencies

- Remove implementation of `Remove` choice from factory templates

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asNumericObservable`, `asTimeObservable`, and `asEvent` implementations were removed)

- Removed `key` from `DateClock`.

### Daml.Finance.Holding

- Update of SDK version and dependencies

- Remove implementation of `Remove` choice from factory templates

- Added default `splitImpl` and `mergeImpl` for `Fungible` to `Util.daml`

- Generalized the `acquireImpl` and `releaseImpl` to not rely on an attribute called "lock"

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure`, `asBase`, and `asTransferable` implementations were removed)

- The `Transfer` choice of the `Transferable` interface now includes the new owner as a choice observer

- Implementation of `Lockable` does not allow an empty `lockers` set

### Daml.Finance.Instrument.Bond

- Update of SDK version and dependencies

- The `Create` choice on the instrument factories returns the corresponding interface (rather than the base instrument interface)

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed)

- Introduce a new callable bond instrument

- Add a `notional` field to all instruments

### Daml.Finance.Instrument.Equity

- Update of SDK version and dependencies

- The `Create` choice on the instrument factory returns the corresponding interface (rather than the base instrument interface)

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed)

- Rename `DeclareDividend` to `DeclareDistribution`

### Daml.Finance.Instrument.Generic

- Update of SDK version and dependencies

- The `Create` choice on the instrument factory returns the corresponding interface (rather than the base instrument interface)

- Move the `Election` module to the `Lifecycle` package. Also, refactor the `Election` to identify the elected sub-tree by a textual tag rather than the actual sub-tree

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBaseInstrument`, and `asClaim` implementations were removed)

- A new instrument version is created and returned by the lifecycle rule choice also when the instrument expires

### Daml.Finance.Instrument.Option

- Update of SDK version and dependencies

- The `Create` choice on the instrument factories returns the corresponding interface (rather than the base instrument interface)

- Add instruments physically-settled European options, dividend options, barrier options

- Renamed cash-settled European options to `EuropeanCash`

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed)

### Daml.Finance.Instrument.Swap

- Update of SDK version and dependencies

- The `Create` choice on the instrument factories returns the corresponding interface (rather than the base instrument interface)

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` implementations were removed)

- `FpmlSwap` now accepts a non-zero rate fixing lag

### Daml.Finance.Instrument.Token

- Update of SDK version and dependencies

- The `Create` choice on the instrument factory returns the corresponding interface (rather than the base instrument interface)

- Make use of the `requires` keyword to enforce the interface hierarchy (in the particular `asDisclosure` and
  `asBaseInstrument` implementations were removed)

### Daml.Finance.Interface.Account

- Update of SDK version and dependencies

- Remove type synonym for `AccountKey`

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` method was removed)

### Daml.Finance.Interface.Claims

- Update of SDK version and dependencies

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asBaseInstrument` method was removed)

### Daml.Finance.Interface.Data

- Update of SDK version and dependencies

- Remove implementation of `Remove` choice from factory templates

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asNumericObservable`, and `asTimeObservable` methods were removed)

### Daml.Finance.Interface.Holding

- Update of SDK version and dependencies

- Remove implementation of `Remove` choice from factory templates

- Removed unnecessary `ArchiveFungible` choice

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBase`, and `asTransferable` methods were removed)

- Fix to signature of `disclose` (removed the `actor` argument)

### Daml.Finance.Interface.Instrument.Base

- Update of SDK version and dependencies

- Removed type synonym for `InstrumentKey`

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` method was removed)

### Daml.Finance.Interface.Instrument.Bond

- Update of SDK version and dependencies

- The `Create` choice on the instrument factories returns the corresponding interface (rather than the base instrument interface)

- Add `GetView` choice to all instrument interfaces

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed)

- Introduce a new callable bond instrument

- Add a `notional` field to all instruments

### Daml.Finance.Interface.Instrument.Equity

- Update of SDK version and dependencies

- The `Create` choice on the instrument factory returns the corresponding interface (rather than the base instrument interface)

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed)

- Rename `DeclareDividend` to `DeclareDistribution`

### Daml.Finance.Interface.Instrument.Generic

- Update of SDK version and dependencies

- The `Create` choice on the instrument factory returns the corresponding interface (rather than the base instrument interface)

- Move the `Election` module to the `Lifecycle` package. Also, refactor the `Election` to identify the elected sub-tree by a textual tag rather than the actual sub-tree

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBaseInstrument`, and `asClaim` methods were removed)

### Daml.Finance.Interface.Instrument.Option

- Update of SDK version and dependencies

- The `Create` choice on the instrument factories returns the corresponding interface (rather than the base instrument interface)

- Add instruments physically-settled European options, dividend options, barrier options

- Renamed cash-settled European options to `EuropeanCash`

- Added `GetView` choice to all instrument interfaces

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure`,
  `asBaseInstrument`, and `asEvent` methods were removed)

### Daml.Finance.Interface.Instrument.Swap

- Update of SDK version and dependencies

- The `Create` choice on the instrument factories returns the corresponding interface (rather than the base instrument interface)

- Added `GetView` choice to all instrument interfaces

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asBaseInstrument` methods were removed)

### Daml.Finance.Interface.Instrument.Token

- Update of SDK version and dependencies

- The `Create` choice on the instrument factory returns the corresponding interface (rather than the base instrument interface)

- Make use of the `requires` keyword to enforce the interface hierarchy (in the particular `asDisclosure` and
  `asBaseInstrument` implementations were removed)

### Daml.Finance.Interface.Lifecycle

- Update of SDK version and dependencies

- Remove implementation of `Remove` choice from factory interfaces

- Move the `Election` module from the `Generic` to the `Lifecycle` package

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asEvent` methods were removed)

### Daml.Finance.Interface.Settlement

- Update of SDK version and dependencies

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` was removed)

### Daml.Finance.Interface.Types.Common

- Update of SDK version and dependencies

### Daml.Finance.Interface.Types.Date

- Update of SDK version and dependencies

### Daml.Finance.Interface.Util

- Update of SDK version and dependencies

- Remove `mapWithIndex` utility function

- Remove the `HasImplementation` type class definition

### Daml.Finance.Lifecycle

- Update of SDK version and dependencies

- Remove implementation of `Remove` choice from factory templates

- Move the `Election` module from the `Generic` to the `Lifecycle` package

- `Election` and `ElectionEffect` implement the `Disclosure` interface

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure` and
  `asEvent` implementations were removed)

- The `Distribution` and `Replacement` lifecycle rules check that the target and procued instruments are active

### Daml.Finance.Settlement

- Update of SDK version and dependencies

- In the settlement `Factory`, the id values used for the `Instruction`s were modified to accurately reflect their order within the `Batch`.

- In the `Batch`, the order of the `settledCids` were changed to match the initial order of the
  instructions in the batch.

- Bug fix: replace `groupOn` by `sortAndGroupOn` in the `Instruction` and `IntermediatedStatic` templates

- Lock pledged holding when allocating to an `Instruction`: the pledged holding is locked to the instruction's requestors and the outgoing controllers of the sending account

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the `asDisclosure`
  implementation was removed)

- When an `Allocation` (resp. `Approval`) takes place, a check has been added to ensure that either the
  `sender` or the `custodian` (resp. the `receiver` or the `custodian`) is among the choice authorizers

- When reallocation (resp. re-approval) occurs, it is required that the `signedSenders`
  (resp. `signedReceivers`) of the `Instruction` are part of the authorizing set

- Add additional checks to the pass-through allocation/approval process. Specifically, verify that the specified pass-through `Instruction` is actually part of the `Batch`. These checks detect settlement failures during the allocation/approval stage rather than waiting until settlement occurs.

- Removed the `key` from the `Batch` implementation

### Daml.Finance.Util

- Update of SDK version and dependencies

- Remove the `groupBy` utility function
