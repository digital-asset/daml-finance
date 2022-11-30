.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Daml.Finance.Interface.Settlement
#################################

This package contains the *interface* for settlement. It has the following modules:

- :ref:`RouteProvider <module-daml-finance-interface-settlement-routeprovider-15164>`:
  Interface for providing a discovery mechanism for settlement routes
- :ref:`Instruction <module-daml-finance-interface-settlement-instruction-10970>`:
  Interface for providing a single instruction to transfer an asset at a custodian
- :ref:`Batch <module-daml-finance-interface-settlement-batch-39188>`:
  Interface for atomically executing instructions, i.e., settling `Transferable`\s
- :ref:`Factory <module-daml-finance-interface-settlement-factory-75196>`:
  Interface used to generate a batch and associated instructions
- :ref:`Types <module-daml-finance-interface-settlement-types-44085>`:
  Types required in the settlement process, e.g., `Step`, `RoutedStep`, `Allocation`, and
  `Approval`

The :doc:`Settlement <../../concepts/settlement>` page contains an overview of the settlement
process and explains the relationship between instructions and batches. Check out the
:doc:`Settlement tutorial <../../tutorials/getting-started/settlement>` for a description on how to
use settlement workflow in practice.

The following diagram shows the incoming and outgoing dependencies for this package:

.. image:: ../../images/daml_finance_interface_settlement.png
   :alt: A diagram showing the incoming and outgoing dependencies of the package.