.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Account.V4
=======================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Account
====================

Version 3.0.0
*************

- Update of SDK version and dependencies.

- The Account now uses a key (specifically, a `HoldingFactoryKey`)
  to reference its `Daml.Finance.Interface.Holding.V4.Factory`, rather than a `ContractId`.

- The `Remove` implementation was removed from the `Factory` (it is newly part of the `Account`
  interface).

- The `Account` has been enhanced to implement the `Lockable` interface with the `custodian`
  as required authorizer for executing the `Acquire` choice (an alternative implementation that does
  not utilize `Lockable` remains viable).

Version 2.0.0
*************

- Update of SDK version and dependencies.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure` implementation was removed).

- Use `ensure` to ensure that the set of outgoing controllers is non-empty.

Version 1.0.1
*************

- Dependencies update.
