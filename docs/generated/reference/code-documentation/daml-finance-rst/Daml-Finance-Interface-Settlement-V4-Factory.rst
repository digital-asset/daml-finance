.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-settlement-v4-factory-85379:

Daml.Finance.Interface.Settlement.V4.Factory
============================================

Interfaces
----------

.. _type-daml-finance-interface-settlement-v4-factory-factory-87188:

**interface** `Factory <type-daml-finance-interface-settlement-v4-factory-factory-87188_>`_

  An interface used to generate settlement instructions\.

  **viewtype** `V <type-daml-finance-interface-settlement-v4-factory-v-79086_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-settlement-v4-factory-getview-80809:

    **Choice** `GetView <type-daml-finance-interface-settlement-v4-factory-getview-80809_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-settlement-v4-factory-view-31386_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-settlement-v4-factory-instruct-82391:

    **Choice** `Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391_>`_

    Generate settlement instructions, and a batch for settling them\.

    Controller\: instructor, consenters

    Returns\: (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-batch-i-86753>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-instruction-i-65587>`\])

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instructor
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party instructing settlement (and the creation of the ``Batch`` and ``Instruction``\\s)\.
       * - consenters
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Parties consenting with the ``Batch`` and ``Instruction``\\s being created\.
       * - settlers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Any of the parties can trigger the final settlement\.
       * - id
         - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Batch identifier\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - Batch description\.
       * - contextId
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
         - Identifier to link a batch to a context (e\.g\. the ``Effect`` it originated from)\.
       * - routedSteps
         - \[:ref:`RoutedStep <type-daml-finance-interface-settlement-v4-types-routedstep-26293>`\]
         - Routed settlement steps to instruct\.
       * - settlementTime
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Settlement time (if any)\.

  + **Method instruct \:** `Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-batch-i-86753>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-instruction-i-65587>`\])

    Implementation of the ``Instruct`` choice\.

Data Types
----------

.. _type-daml-finance-interface-settlement-v4-factory-i-2953:

**type** `I <type-daml-finance-interface-settlement-v4-factory-i-2953_>`_
  \= `Factory <type-daml-finance-interface-settlement-v4-factory-factory-87188_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-settlement-v4-factory-v-79086:

**type** `V <type-daml-finance-interface-settlement-v4-factory-v-79086_>`_
  \= `View <type-daml-finance-interface-settlement-v4-factory-view-31386_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-settlement-v4-factory-factory-87188_>`_ `V <type-daml-finance-interface-settlement-v4-factory-v-79086_>`_

.. _type-daml-finance-interface-settlement-v4-factory-view-31386:

**data** `View <type-daml-finance-interface-settlement-v4-factory-view-31386_>`_

  View for ``Factory``\.

  .. _constr-daml-finance-interface-settlement-v4-factory-view-90909:

  `View <constr-daml-finance-interface-settlement-v4-factory-view-90909_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - Party providing the facility\.
       * - observers
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - Observers\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-settlement-v4-factory-view-31386_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-settlement-v4-factory-view-31386_>`_

Functions
---------

.. _function-daml-finance-interface-settlement-v4-factory-instruct-96835:

`instruct <function-daml-finance-interface-settlement-v4-factory-instruct-96835_>`_
  \: `Factory <type-daml-finance-interface-settlement-v4-factory-factory-87188_>`_ \-\> `Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-batch-i-86753>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-instruction-i-65587>`\])
