.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Holding.V4
=======================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Holding
====================

Version 3.0.1
*************

- Resolved an issue in the `Daml.Finance.Holding.V4` package affecting the `Disclosure.I`
  implementation of the `Factory` template within the `Daml.Finance.Holding.V4.Factory` module. This
  patch ensures that observers are now correctly updated for the associated `Reference` template.

Version 3.0.0
*************

- Update of SDK version and dependencies.

- Added an `id : Id` field to the `Factory`.

- Replaced all factories by a single `Factory` for all holding implementations.

- Added an assert that the `custodian` remains the same for both the sending and receiving accounts.

- Removed the consistence check that credited and debited holdings must have the same
  `templateTypeRep`. They now need to have the same `HoldingStandard` (but can have different
  implementations).

- Added a `Fungible` implementation, and renamed the `NonTransferable` and `Fungible`
  implementations to `BaseHolding` and `TransferableFungible`, respectively.

- The holding implementations newly `ensure` that the desired `HoldingStandard` is met.

- The locking logic was factored out to a separate `Lockable` interface (within the
  `Daml.Finance.Interface.Util.V3` package), and the `acquireImpl` and `releaseImpl` utility functions
  moved to the `Lockable` module in the `Daml.Finance.Util.V4` implementation package.

- The `Transfer`, `Split`, `Merge`, and `Debit` actions on holdings are prohibited in a locked
  state, requiring them to be unlocked first. Notably, the type signatures for `splitImpl` and
  `mergeImpl` have been modified, and the re-entrant lock logic of `transferImpl` removed.

- Renamed the `F` type synonym to `T`.

Version 2.0.0
*************

- Update of SDK version and dependencies.

- Remove implementation of `Remove` choice from factory templates.

- Added default `splitImpl` and `mergeImpl` for `Fungible` to `Util.daml`.

- Generalized the `acquireImpl` and `releaseImpl` to not rely on an attribute called "lock".

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure`, `asBase`, and `asTransferable` implementations were removed).

- The `Transfer` choice of the `Transferable` interface now includes the new owner as a choice
  observer.

- Implementation of `Lockable` does not allow an empty `lockers` set.

Version 1.0.2
*************

- Dependencies update.

Version 1.0.1
*************

- Fix bug in the implementation of `Fungible.Merge`.

- Improve error message when acquiring a lock.
