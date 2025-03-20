.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Lifecycle.V4
=========================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Lifecycle
======================

Version 3.0.0
*************

- Update of SDK version and dependencies.

- The `Calculate` choice of the `Effect` and `ElectionEffect` now takes a quantity as argument
  to reflect the change in the `Effect.I` interface. The implementation of the `ClaimEffect` choice
  body of `Daml.Finance.Lifecycle.V4.Rule.Claim` also changed accordingly.

- Replaced `lookupByKey` by an `exerciseByKey` in the `Distribution` and `Replacement` rule.

- Replaced `providers : Parties` with `provider : Party` in the `Claim` rule (i.e., in the
  implementation only).

Version 2.0.0
*************

- Update of SDK version and dependencies.

- Remove implementation of `Remove` choice from factory templates.

- Move the `Election` module from the `Generic` to the `Lifecycle` package.

- `Election` and `ElectionEffect` implement the `Disclosure` interface.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure` and `asEvent` implementations were removed).

- The `Distribution` and `Replacement` lifecycle rules check that the target and procued instruments
  are active.

Version 1.0.1
*************

- Dependencies update.
