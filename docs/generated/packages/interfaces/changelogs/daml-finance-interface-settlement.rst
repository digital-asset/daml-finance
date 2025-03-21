.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Interface.Settlement.V4
====================================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Interface.Settlement
=================================

Version 3.0.0
*************

- Update of SDK version and dependencies.

- For the `Batch` and `Instruction`, the `requestors : Parties` field was split up into an
  `instructor : Party` as maintainer of the `Instruction` key, and `consenters : Parties` as
  additional signatories. The `Batch` and `Instruction` views were amended accordingly.

Version 2.0.0
*************

- Update of SDK version and dependencies.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure` was removed).
