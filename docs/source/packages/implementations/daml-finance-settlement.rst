.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Settlement.V4
##########################

This package contains the *implementation* of the components used for settlement. It has the
following modules:

- :ref:`RouteProvider.SingleCustodian <module-daml-finance-settlement-v4-routeprovider-singlecustodian-88974>`:
  Used to generate a single `RoutedStep` from a `Step` using a single custodian
- :ref:`RouteProvider.IntermediatedStatic <module-daml-finance-settlement-v4-routeprovider-intermediatedstatic-92315>`:
  Used to generate a route, i.e., `RoutedStep`\s, for each settlement `Step`
- :ref:`Instruction <module-daml-finance-settlement-v4-instruction-73130>`: Used to settle a single
  `RoutedStep`, i.e., a `Step` at a custodian.
- :ref:`Batch <module-daml-finance-settlement-v4-batch-88124>`: Allows you to atomically settle a
  set of settlement `Instruction`\s
- :ref:`Factory <module-daml-finance-settlement-v4-factory-65040>`: Used to create a set of
  settlement `Instruction`\s, and a `Batch` to atomically settle them
- :ref:`Hierarchy <module-daml-finance-settlement-v4-hierarchy-83331>`: Data type that describes a
  hierarchical account structure between multiple parties

The :doc:`Settlement <../../concepts/settlement>` page contains an overview of the settlement
process and explains the relationship between ``Instruction`` and ``Batch``. Also, check out the
:doc:`Settlement tutorial <../../tutorials/getting-started/settlement>` for a description on how to
implement the settlement workflow in practice.

Changelog
*********
