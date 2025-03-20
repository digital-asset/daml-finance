.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-settlement-v4-batch-88124:

Daml.Finance.Settlement.V4.Batch
================================

Templates
---------

.. _type-daml-finance-settlement-v4-batch-batch-9941:

**template** `Batch <type-daml-finance-settlement-v4-batch-batch-9941_>`_

  Allows you to atomically settle a set of settlement ``Step``\.

  Signatory\: instructor, consenters

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - instructor
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Party instructing settlement (and the creation of the ``Batch``)\.
     * - consenters
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Parties consenting with the creation of the ``Batch``\.
     * - settlers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Any of the parties can trigger the settlement\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Batch identifier\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Batch description\.
     * - contextId
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Identifier to link a batch to a context (e\.g\. the ``Effect`` it originated from)\.
     * - routedStepsWithInstructionId
       - \[(:ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`, :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`)\]
       - The settlement ``RoutedStep``\\s and the identifiers of the corresponding ``Instruction``\\s\.
     * - settlementTime
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Settlement time (if any)\.

  + **Choice** Archive

    Controller\: instructor, consenters

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-settlement-v4-batch-i-86753>` **for** `Batch <type-daml-finance-settlement-v4-batch-batch-9941_>`_

Data Types
----------

.. _type-daml-finance-settlement-v4-batch-t-21205:

**type** `T <type-daml-finance-settlement-v4-batch-t-21205_>`_
  \= `Batch <type-daml-finance-settlement-v4-batch-batch-9941_>`_

  Type synonym for ``Batch``\.
