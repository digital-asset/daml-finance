.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-generic-v4-factory-73556:

Daml.Finance.Interface.Instrument.Generic.V4.Factory
====================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-generic-v4-factory-factory-8845:

**interface** `Factory <type-daml-finance-interface-instrument-generic-v4-factory-factory-8845_>`_

  Factory interface to instantiate generic instruments using Contingent Claims\.

  **viewtype** `V <type-daml-finance-interface-instrument-generic-v4-factory-v-43939_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-generic-v4-factory-create-52332:

    **Choice** `Create <type-daml-finance-interface-instrument-generic-v4-factory-create-52332_>`_

    Create a new generic instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" instrument), (DA\.Internal\.Record\.getField @\"issuer\" instrument)

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-generic-v4-instrument-i-8248>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument's key\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A description of the instrument\.
       * - claims
         - :ref:`C <type-daml-finance-interface-claims-v4-types-c-76802>`
         - The claim tree\.
       * - acquisitionTime
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - The claim's acquisition time\. This usually corresponds to the start date of the contract\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-generic-v4-factory-create-52332_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-generic-v4-instrument-i-8248>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-generic-v4-factory-i-33172:

**type** `I <type-daml-finance-interface-instrument-generic-v4-factory-i-33172_>`_
  \= `Factory <type-daml-finance-interface-instrument-generic-v4-factory-factory-8845_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-generic-v4-factory-v-43939:

**type** `V <type-daml-finance-interface-instrument-generic-v4-factory-v-43939_>`_
  \= `View <type-daml-finance-interface-instrument-generic-v4-factory-view-99113_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-generic-v4-factory-factory-8845_>`_ `V <type-daml-finance-interface-instrument-generic-v4-factory-v-43939_>`_

.. _type-daml-finance-interface-instrument-generic-v4-factory-view-99113:

**data** `View <type-daml-finance-interface-instrument-generic-v4-factory-view-99113_>`_

  .. _constr-daml-finance-interface-instrument-generic-v4-factory-view-44894:

  `View <constr-daml-finance-interface-instrument-generic-v4-factory-view-44894_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-generic-v4-factory-view-99113_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-generic-v4-factory-view-99113_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-generic-v4-factory-createtick-63852:

`create' <function-daml-finance-interface-instrument-generic-v4-factory-createtick-63852_>`_
  \: `Factory <type-daml-finance-interface-instrument-generic-v4-factory-factory-8845_>`_ \-\> `Create <type-daml-finance-interface-instrument-generic-v4-factory-create-52332_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-generic-v4-instrument-i-8248>`)
