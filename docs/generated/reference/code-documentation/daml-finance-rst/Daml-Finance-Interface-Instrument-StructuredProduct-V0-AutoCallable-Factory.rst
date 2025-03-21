.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-50965:

Daml.Finance.Interface.Instrument.StructuredProduct.V0.AutoCallable.Factory
===========================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-factory-17934:

**interface** `Factory <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-factory-17934_>`_

  Factory interface to instantiate AutoCallable instruments\.

  **viewtype** `V <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-v-6824_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-create-58961:

    **Choice** `Create <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-create-58961_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" autoCallable)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" autoCallable))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-i-91905>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - autoCallable
         - :ref:`AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435>`
         - Attributes to create an AutoCallable\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-create-58961_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-i-91905>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-i-21871:

**type** `I <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-i-21871_>`_
  \= `Factory <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-factory-17934_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-v-6824:

**type** `V <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-v-6824_>`_
  \= `View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-view-74308_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-factory-17934_>`_ `V <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-v-6824_>`_

.. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-view-74308:

**data** `View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-view-74308_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-view-37425:

  `View <constr-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-view-37425_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-view-74308_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-view-74308_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-createtick-59943:

`create' <function-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-createtick-59943_>`_
  \: `Factory <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-factory-17934_>`_ \-\> `Create <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-create-58961_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-i-91905>`)
