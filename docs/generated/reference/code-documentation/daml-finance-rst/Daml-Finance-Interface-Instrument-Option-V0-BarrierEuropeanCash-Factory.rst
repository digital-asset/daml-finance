.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-3874:

Daml.Finance.Interface.Instrument.Option.V0.BarrierEuropeanCash.Factory
=======================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-factory-30599:

**interface** `Factory <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-factory-30599_>`_

  Factory interface to instantiate barrier options\.

  **viewtype** `V <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-v-60669_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-create-44698:

    **Choice** `Create <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-create-44698_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" barrierEuropean)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" barrierEuropean))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-i-62014>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - barrierEuropean
         - :ref:`BarrierEuropean <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-types-barriereuropean-83436>`
         - Attributes to create a barrier option\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-create-44698_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-i-62014>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-i-63002:

**type** `I <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-i-63002_>`_
  \= `Factory <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-factory-30599_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-v-60669:

**type** `V <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-v-60669_>`_
  \= `View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-view-50371_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-factory-30599_>`_ `V <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-v-60669_>`_

.. _type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-view-50371:

**data** `View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-view-50371_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-view-98914:

  `View <constr-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-view-98914_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-view-50371_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-view-50371_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-createtick-37818:

`create' <function-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-createtick-37818_>`_
  \: `Factory <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-factory-30599_>`_ \-\> `Create <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-factory-create-44698_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-i-62014>`)
