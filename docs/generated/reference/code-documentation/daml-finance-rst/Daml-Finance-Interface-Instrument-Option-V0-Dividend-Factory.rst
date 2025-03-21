.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-dividend-factory-71631:

Daml.Finance.Interface.Instrument.Option.V0.Dividend.Factory
============================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-option-v0-dividend-factory-factory-81964:

**interface** `Factory <type-daml-finance-interface-instrument-option-v0-dividend-factory-factory-81964_>`_

  Factory interface to instantiate Dividend options\.

  **viewtype** `V <type-daml-finance-interface-instrument-option-v0-dividend-factory-v-22022_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-option-v0-dividend-factory-create-44747:

    **Choice** `Create <type-daml-finance-interface-instrument-option-v0-dividend-factory-create-44747_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" dividend)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" dividend))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-option-v0-dividend-instrument-i-22979>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - dividend
         - :ref:`Dividend <type-daml-finance-interface-instrument-option-v0-dividend-types-dividend-7997>`
         - Attributes to create a Dividend option\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-option-v0-dividend-factory-create-44747_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-option-v0-dividend-instrument-i-22979>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-option-v0-dividend-factory-i-15409:

**type** `I <type-daml-finance-interface-instrument-option-v0-dividend-factory-i-15409_>`_
  \= `Factory <type-daml-finance-interface-instrument-option-v0-dividend-factory-factory-81964_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-option-v0-dividend-factory-v-22022:

**type** `V <type-daml-finance-interface-instrument-option-v0-dividend-factory-v-22022_>`_
  \= `View <type-daml-finance-interface-instrument-option-v0-dividend-factory-view-850_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-option-v0-dividend-factory-factory-81964_>`_ `V <type-daml-finance-interface-instrument-option-v0-dividend-factory-v-22022_>`_

.. _type-daml-finance-interface-instrument-option-v0-dividend-factory-view-850:

**data** `View <type-daml-finance-interface-instrument-option-v0-dividend-factory-view-850_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-option-v0-dividend-factory-view-93797:

  `View <constr-daml-finance-interface-instrument-option-v0-dividend-factory-view-93797_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-option-v0-dividend-factory-view-850_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-option-v0-dividend-factory-view-850_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-option-v0-dividend-factory-createtick-45617:

`create' <function-daml-finance-interface-instrument-option-v0-dividend-factory-createtick-45617_>`_
  \: `Factory <type-daml-finance-interface-instrument-option-v0-dividend-factory-factory-81964_>`_ \-\> `Create <type-daml-finance-interface-instrument-option-v0-dividend-factory-create-44747_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-option-v0-dividend-instrument-i-22979>`)
