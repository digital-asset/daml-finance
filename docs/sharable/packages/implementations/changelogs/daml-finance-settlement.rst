.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Changelog
#########

Daml.Finance.Settlement.V4
==========================

Version 4.0.0
*************

- Update of SDK version and dependencies. LF protocol update to support SCU.

Daml.Finance.Settlement
=======================

Version 3.0.0
*************

- Update of SDK version and dependencies.

- Removed the check for consistent settled holdings. Now, holdings of the same instrument don't
  need identical `templateTypeRep`, instead they should share the same token standard
  (implementation variations are allowed).

- For the `Batch` and `Instruction` templates, the `requestors : Parties` field got split up into
  an `instructor : Party` as maintainer of the `Instruction` key, and `consenters : Parties` as
  additional signatories.

Version 2.0.0
*************

- Update of SDK version and dependencies.

- In the settlement `Factory`, the id values used for the `Instruction`s were modified to accurately
  reflect their order within the `Batch`.

- In the `Batch`, the order of the `settledCids` were changed to match the initial order of the
  instructions in the batch.

- Bug fix: replace `groupOn` by `sortAndGroupOn` in the `Instruction` and `IntermediatedStatic`
  templates.

- Lock pledged holding when allocating to an `Instruction`: the pledged holding is locked to the
  instruction's requestors and the outgoing controllers of the sending account.

- Make use of the `requires` keyword to enforce the interface hierarchy (in particular the
  `asDisclosure` implementation was removed).

- When an `Allocation` (resp. `Approval`) takes place, a check has been added to ensure that either
  the `sender` or the `custodian` (resp. the `receiver` or the `custodian`) is among the choice
  authorizers.

- When reallocation (resp. re-approval) occurs, it is required that the `signedSenders`
  (resp. `signedReceivers`) of the `Instruction` are part of the authorizing set.

- Add additional checks to the pass-through allocation/approval process. Specifically, verify that
  the specified pass-through `Instruction` is actually part of the `Batch`. These checks detect
  settlement failures during the allocation/approval stage rather than waiting until settlement
  occurs.

- Removed the `key` from the `Batch` implementation.

Version 1.0.2
*************

- Dependencies update.

Version 1.0.1
*************

- Additional sanity checks added to `Instruction`.
