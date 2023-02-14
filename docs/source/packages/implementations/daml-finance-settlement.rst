.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Settlement
#######################

This package contains the *implementation* of the components used for settlement. It has the
following modules:

- :ref:`RouteProvider.SingleCustodian <module-daml-finance-settlement-routeprovider-singlecustodian-83455>`:
  Used to generate a single `RoutedStep` from a `Step` using a single custodian
- :ref:`RouteProvider.IntermediatedStatic <module-daml-finance-settlement-routeprovider-intermediatedstatic-17490>`:
  Used to generate a route, i.e., `RoutedStep`\s, for each settlement `Step`
- :ref:`Instruction <module-daml-finance-settlement-instruction-87187>`: Used to settle a single
  `RoutedStep`, i.e., a `Step` at a custodian.
- :ref:`Batch <module-daml-finance-settlement-batch-95573>`: Allows you to atomically settle a
  set of settlement `Instruction`\s
- :ref:`Factory <module-daml-finance-settlement-factory-257>`: Used to create a set of
  settlement `Instruction`\s, and a `Batch` to atomically settle them
- :ref:`Hierarchy <module-daml-finance-settlement-hierarchy-15826>`: Data type that describes a
  hierarchical account structure among multiple parties

The :doc:`Settlement <../../concepts/settlement>` page contains an overview of the settlement
process and explains the relationship between ``Instruction`` and ``Batch``. Also, check out the
:doc:`Settlement tutorial <../../tutorials/getting-started/settlement>` for a description on how to
implement the settlement workflow in practice.

Changelog
*********

.. toctree::
   :titlesonly:
   :maxdepth: 1

   Changelog <changelogs/daml-finance-settlement>
