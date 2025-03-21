.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-91145:

Daml.Finance.Interface.Instrument.StructuredProduct.V0.BarrierReverseConvertible.Factory
========================================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-factory-47150:

**interface** `Factory <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-factory-47150_>`_

  Factory interface to instantiate BRCs\.

  **viewtype** `V <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-v-45512_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-create-85905:

    **Choice** `Create <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-create-85905_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" barrierReverseConvertible)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" barrierReverseConvertible))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-i-56857>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - barrierReverseConvertible
         - :ref:`BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687>`
         - Attributes to create a BRC\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-create-85905_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-i-56857>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-i-60559:

**type** `I <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-i-60559_>`_
  \= `Factory <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-factory-47150_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-v-45512:

**type** `V <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-v-45512_>`_
  \= `View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-view-61060_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-factory-47150_>`_ `V <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-v-45512_>`_

.. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-view-61060:

**data** `View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-view-61060_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-view-2195:

  `View <constr-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-view-2195_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-view-61060_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-view-61060_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-createtick-74039:

`create' <function-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-createtick-74039_>`_
  \: `Factory <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-factory-47150_>`_ \-\> `Create <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-create-85905_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-i-56857>`)
