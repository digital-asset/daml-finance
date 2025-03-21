.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-holding-v4-holding-20535:

Daml.Finance.Interface.Holding.V4.Holding
=========================================

Interfaces
----------

.. _type-daml-finance-interface-holding-v4-holding-holding-66887:

**interface** `Holding <type-daml-finance-interface-holding-v4-holding-holding-66887_>`_

  Holding interface\.

  **viewtype** `V <type-daml-finance-interface-holding-v4-holding-v-1774_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-holding-v4-holding-getview-9417:

    **Choice** `GetView <type-daml-finance-interface-holding-v4-holding-getview-9417_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-holding-v4-holding-v-1774_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.


Data Types
----------

.. _type-daml-finance-interface-holding-v4-holding-i-25641:

**type** `I <type-daml-finance-interface-holding-v4-holding-i-25641_>`_
  \= `Holding <type-daml-finance-interface-holding-v4-holding-holding-66887_>`_

  Type synonym for ``Holding``\.

  **instance** HasMethod :ref:`Account <type-daml-finance-interface-account-v4-account-account-93407>` \"credit\" (:ref:`Credit <type-daml-finance-interface-account-v4-account-credit-92816>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-holding-i-25641_>`_))

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-holding-v4-factory-factory-22859>` \"create'\" (:ref:`Create <type-daml-finance-interface-holding-v4-factory-create-84550>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-holding-i-25641_>`_))

  **instance** HasMethod :ref:`Batch <type-daml-finance-interface-settlement-v4-batch-batch-20548>` \"cancel\" (:ref:`Cancel <type-daml-finance-interface-settlement-v4-batch-cancel-13653>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-holding-i-25641_>`_\])

  **instance** HasMethod :ref:`Batch <type-daml-finance-interface-settlement-v4-batch-batch-20548>` \"settle\" (:ref:`Settle <type-daml-finance-interface-settlement-v4-batch-settle-93506>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-holding-i-25641_>`_\])

  **instance** HasMethod :ref:`Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168>` \"allocate\" (:ref:`Allocate <type-daml-finance-interface-settlement-v4-instruction-allocate-48530>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168>`, `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-holding-i-25641_>`_)))

  **instance** HasMethod :ref:`Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168>` \"cancel\" (:ref:`Cancel <type-daml-finance-interface-settlement-v4-instruction-cancel-2291>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-holding-i-25641_>`_)))

  **instance** HasMethod :ref:`Instruction <type-daml-finance-interface-settlement-v4-instruction-instruction-59168>` \"execute\" (:ref:`Execute <type-daml-finance-interface-settlement-v4-instruction-execute-24017>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-holding-v4-holding-i-25641_>`_)))

.. _type-daml-finance-interface-holding-v4-holding-v-1774:

**type** `V <type-daml-finance-interface-holding-v4-holding-v-1774_>`_
  \= `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Holding <type-daml-finance-interface-holding-v4-holding-holding-66887_>`_ `V <type-daml-finance-interface-holding-v4-holding-v-1774_>`_

.. _type-daml-finance-interface-holding-v4-holding-view-10906:

**data** `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_

  View for ``Holding``\.

  .. _constr-daml-finance-interface-holding-v4-holding-view-99999:

  `View <constr-daml-finance-interface-holding-v4-holding-view-99999_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - Instrument being held\.
       * - account
         - :ref:`AccountKey <type-daml-finance-interface-types-common-v3-types-accountkey-55962>`
         - Key of the account holding the assets\.
       * - amount
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - Size of the holding\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-holding-v4-holding-view-10906_>`_
