.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-settlement-v4-instruction-73130:

Daml.Finance.Settlement.V4.Instruction
======================================

Templates
---------

.. _type-daml-finance-settlement-v4-instruction-instruction-65077:

**template** `Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077_>`_

  Instruction is used to settle a single settlement ``Step``\. In order to settle the instruction,

  * the sender must allocate a suitable holding
  * the receiver must define the receiving account

  Signatory\: instructor, consenters, signedSenders, signedReceivers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - instructor
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party instructing settlement (and the creation of the ``Instruction``)\.
     * - consenters
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Parties consenting with the creation of the ``Instruction``\.
     * - settlers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Any of the parties can trigger the settlement\.
     * - batchId
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Trade identifier\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Instruction identifier\.
     * - routedStep
       - :ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`
       - Routed settlement step\.
     * - settlementTime
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Settlement time (if any)\.
     * - allocation
       - :ref:`Allocation <type-daml-finance-interface-settlement-v4-types-allocation-41200>`
       - Allocation from the sender\.
     * - approval
       - :ref:`Approval <type-daml-finance-interface-settlement-v4-types-approval-77821>`
       - Approval from the receiver\.
     * - signedSenders
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Additional signatories, used to collect authorization\.
     * - signedReceivers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Additional signatories, used to collect authorization\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.

  + **Choice** Archive

    Controller\: instructor, consenters, signedSenders, signedReceivers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-settlement-v4-instruction-i-65587>` **for** `Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077_>`_

Data Types
----------

.. _type-daml-finance-settlement-v4-instruction-t-83203:

**type** `T <type-daml-finance-settlement-v4-instruction-t-83203_>`_
  \= `Instruction <type-daml-finance-settlement-v4-instruction-instruction-65077_>`_

  Type synonym for ``Instruction``\.
