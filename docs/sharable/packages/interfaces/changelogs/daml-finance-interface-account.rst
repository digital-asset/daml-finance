.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Interface.Account.V4
=================================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Interface.Account
==============================

Version 3.0.0
*************

- Update of SDK version and dependencies.

- Removed the `ContractId Holding.Factory` from the account view.

- The `Remove` choice, which was previously a choice of the `Factory`, has now been reassigned to
  the `Account`.

- The `Create` choice of the account's `Factory` has been adapted; it now takes a
  `HoldingFactoryKey` instead of the `ContractId Daml.Finance.Interface.Holding.V4.Factory` as input

- Renamed the `F` type synonym to `I` .

Version 2.0.0
*************

- Update of SDK version and dependencies.

- Remove type synonym for `AccountKey`.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure` method was removed).