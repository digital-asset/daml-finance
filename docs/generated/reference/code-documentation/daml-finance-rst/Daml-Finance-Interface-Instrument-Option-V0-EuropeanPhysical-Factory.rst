.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-option-v0-europeanphysical-factory-14616:

Daml.Finance.Interface.Instrument.Option.V0.EuropeanPhysical.Factory
====================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-factory-2909:

**interface** `Factory <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-factory-2909_>`_

  Factory interface to instantiate European options\.

  **viewtype** `V <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-v-71507_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-create-41404:

    **Choice** `Create <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-create-41404_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" european)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" european))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-option-v0-europeanphysical-instrument-i-88456>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - european
         - :ref:`European <type-daml-finance-interface-instrument-option-v0-europeanphysical-types-european-81104>`
         - Attributes to create a European option\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-create-41404_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-option-v0-europeanphysical-instrument-i-88456>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-i-99780:

**type** `I <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-i-99780_>`_
  \= `Factory <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-factory-2909_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-v-71507:

**type** `V <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-v-71507_>`_
  \= `View <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-view-82873_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-factory-2909_>`_ `V <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-v-71507_>`_

.. _type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-view-82873:

**data** `View <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-view-82873_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-option-v0-europeanphysical-factory-view-4910:

  `View <constr-daml-finance-interface-instrument-option-v0-europeanphysical-factory-view-4910_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-view-82873_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-view-82873_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-option-v0-europeanphysical-factory-createtick-84252:

`create' <function-daml-finance-interface-instrument-option-v0-europeanphysical-factory-createtick-84252_>`_
  \: `Factory <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-factory-2909_>`_ \-\> `Create <type-daml-finance-interface-instrument-option-v0-europeanphysical-factory-create-41404_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-option-v0-europeanphysical-instrument-i-88456>`)
