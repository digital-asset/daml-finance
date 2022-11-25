.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Settlement
#######################

This package contains the *implementation* of the components used for settlement. It has the
following modules:

- :ref:`Instruction <module-daml-finance-settlement-instruction-87187>`: Used to settle a single
  settlement `Step` at custodian (i.e., a `RoutedStep`)
- :ref:`Batch <module-daml-finance-settlement-batch-95573>`: Allows you to atomically settle a
  set of settlement `Instruction`\s
- :ref:`Factory <module-daml-finance-settlement-factory-257>`: Used to create a set of
  settlement `Instruction`\s, and a `Batch` to atomically settle them

The :doc:`Settlement <../../concepts/settlement>` page contains an overview of the settlement
process and explains the relationship between ``Instruction`` and ``Batch``. Check out the
:doc:`Settlement tutorial <../../tutorials/getting-started/settlement>` for a description on how to
implement the settlement workflow in practice.
