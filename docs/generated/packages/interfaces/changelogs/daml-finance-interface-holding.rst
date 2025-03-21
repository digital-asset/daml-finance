.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Interface.Holding.V4
=================================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Interface.Holding
==============================

Version 3.0.0
*************

- Update of SDK version and dependencies.

- Factored out the locking logic from the base `Holding` interface to a separate interface called
  `Lockable` of the `Daml.Finance.Interface.Util.V3` package.

- Updated the `Daml.Finance.Interface.Holding.V4.Factory` to use a key, employing a `Reference`
  template and the `HoldingFactoryKey` data type. Additionally, It also requires the `Disclosure.I`
  and has a `getKey` method and `Remove` choice.

- Removed the requirement that a `Fungible.I` requires `Transferable.I`.

- Renamed `Base` to `Holding`.

- Renamed the `F` type synonym to `I`.

Version 2.0.0
*************

- Update of SDK version and dependencies.

- Remove implementation of `Remove` choice from factory templates.

- Removed unnecessary `ArchiveFungible` choice.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure`, `asBase`, and `asTransferable` methods were removed).

- Fix to signature of `disclose` (removed the `actor` argument).
