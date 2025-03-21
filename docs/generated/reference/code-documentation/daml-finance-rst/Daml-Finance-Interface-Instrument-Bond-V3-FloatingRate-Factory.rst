.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-floatingrate-factory-1777:

Daml.Finance.Interface.Instrument.Bond.V3.FloatingRate.Factory
==============================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-factory-65490:

**interface** `Factory <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-factory-65490_>`_

  Factory interface to instantiate floating\-rate bond instruments\.

  **viewtype** `V <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-v-38996_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-create-37181:

    **Choice** `Create <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-create-37181_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" floatingRate)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" floatingRate))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-floatingrate-instrument-i-29309>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - floatingRate
         - :ref:`FloatingRate <type-daml-finance-interface-instrument-bond-v3-floatingrate-types-floatingrate-91442>`
         - Attributes to create a floating rate bond\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-create-37181_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-floatingrate-instrument-i-29309>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-i-49763:

**type** `I <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-i-49763_>`_
  \= `Factory <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-factory-65490_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-v-38996:

**type** `V <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-v-38996_>`_
  \= `View <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-view-68416_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-factory-65490_>`_ `V <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-v-38996_>`_

.. _type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-view-68416:

**data** `View <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-view-68416_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-bond-v3-floatingrate-factory-view-13451:

  `View <constr-daml-finance-interface-instrument-bond-v3-floatingrate-factory-view-13451_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-view-68416_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-view-68416_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-bond-v3-floatingrate-factory-createtick-87699:

`create' <function-daml-finance-interface-instrument-bond-v3-floatingrate-factory-createtick-87699_>`_
  \: `Factory <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-factory-65490_>`_ \-\> `Create <type-daml-finance-interface-instrument-bond-v3-floatingrate-factory-create-37181_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-floatingrate-instrument-i-29309>`)
