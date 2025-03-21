.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-settlement-v4-batch-88127:

Daml.Finance.Interface.Settlement.V4.Batch
==========================================

Interfaces
----------

.. _type-daml-finance-interface-settlement-v4-batch-batch-20548:

**interface** `Batch <type-daml-finance-interface-settlement-v4-batch-batch-20548_>`_

  An interface for atomically settling a batch of ``Instruction``\\s\.
  The corresponding Instructions are referenced by key\.

  **viewtype** `V <type-daml-finance-interface-settlement-v4-batch-v-54326_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-settlement-v4-batch-cancel-13653:

    **Choice** `Cancel <type-daml-finance-interface-settlement-v4-batch-cancel-13653_>`_

    Cancels the batch\.

    Controller\: actors

    Returns\: \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`\]

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - The parties canceling the batch\.

  + .. _type-daml-finance-interface-settlement-v4-batch-getview-66369:

    **Choice** `GetView <type-daml-finance-interface-settlement-v4-batch-getview-66369_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-settlement-v4-batch-view-11618_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.

  + .. _type-daml-finance-interface-settlement-v4-batch-settle-93506:

    **Choice** `Settle <type-daml-finance-interface-settlement-v4-batch-settle-93506_>`_

    Settles the batch\.

    Controller\: actors

    Returns\: \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`\]

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - actors
         - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
         - The parties settling the batch\.

  + **Method cancel \:** `Cancel <type-daml-finance-interface-settlement-v4-batch-cancel-13653_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`\]

    Implementation of the ``Cancel`` choice\.

  + **Method settle \:** `Settle <type-daml-finance-interface-settlement-v4-batch-settle-93506_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`\]

    Implementation of the ``Settle`` choice\.

Data Types
----------

.. _type-daml-finance-interface-settlement-v4-batch-i-86753:

**type** `I <type-daml-finance-interface-settlement-v4-batch-i-86753_>`_
  \= `Batch <type-daml-finance-interface-settlement-v4-batch-batch-20548_>`_

  Type synonym for ``Batch``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-settlement-v4-factory-factory-87188>` \"instruct\" (:ref:`Instruct <type-daml-finance-interface-settlement-v4-factory-instruct-82391>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-settlement-v4-batch-i-86753_>`_, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-settlement-v4-instruction-i-65587>`\]))

.. _type-daml-finance-interface-settlement-v4-batch-v-54326:

**type** `V <type-daml-finance-interface-settlement-v4-batch-v-54326_>`_
  \= `View <type-daml-finance-interface-settlement-v4-batch-view-11618_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Batch <type-daml-finance-interface-settlement-v4-batch-batch-20548_>`_ `V <type-daml-finance-interface-settlement-v4-batch-v-54326_>`_

.. _type-daml-finance-interface-settlement-v4-batch-view-11618:

**data** `View <type-daml-finance-interface-settlement-v4-batch-view-11618_>`_

  View for ``Batch``\.

  .. _constr-daml-finance-interface-settlement-v4-batch-view-60917:

  `View <constr-daml-finance-interface-settlement-v4-batch-view-60917_>`_

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
         - Parties that can trigger the final settlement\.
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
         - Routed settlement steps\.
       * - settlementTime
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - Settlement time (if any)\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-settlement-v4-batch-view-11618_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-settlement-v4-batch-view-11618_>`_

Functions
---------

.. _function-daml-finance-interface-settlement-v4-batch-settle-38134:

`settle <function-daml-finance-interface-settlement-v4-batch-settle-38134_>`_
  \: `Batch <type-daml-finance-interface-settlement-v4-batch-batch-20548_>`_ \-\> `Settle <type-daml-finance-interface-settlement-v4-batch-settle-93506_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`\]

.. _function-daml-finance-interface-settlement-v4-batch-cancel-24825:

`cancel <function-daml-finance-interface-settlement-v4-batch-cancel-24825_>`_
  \: `Batch <type-daml-finance-interface-settlement-v4-batch-batch-20548_>`_ \-\> `Cancel <type-daml-finance-interface-settlement-v4-batch-cancel-13653_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-holding-v4-holding-i-25641>`\]
