.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-equity-v0-factory-67939:

Daml.Finance.Interface.Instrument.Equity.V0.Factory
===================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-equity-v0-factory-factory-21456:

**interface** `Factory <type-daml-finance-interface-instrument-equity-v0-factory-factory-21456_>`_

  Factory interface to instantiate equities\.

  **viewtype** `V <type-daml-finance-interface-instrument-equity-v0-factory-v-99362_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-equity-v0-factory-create-45111:

    **Choice** `Create <type-daml-finance-interface-instrument-equity-v0-factory-create-45111_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" instrument), (DA\.Internal\.Record\.getField @\"issuer\" instrument)

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-equity-v0-instrument-i-47875>`

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
       * - validAsOf
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-equity-v0-factory-create-45111_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-equity-v0-instrument-i-47875>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-equity-v0-factory-i-27509:

**type** `I <type-daml-finance-interface-instrument-equity-v0-factory-i-27509_>`_
  \= `Factory <type-daml-finance-interface-instrument-equity-v0-factory-factory-21456_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-equity-v0-factory-v-99362:

**type** `V <type-daml-finance-interface-instrument-equity-v0-factory-v-99362_>`_
  \= `View <type-daml-finance-interface-instrument-equity-v0-factory-view-61438_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-equity-v0-factory-factory-21456_>`_ `V <type-daml-finance-interface-instrument-equity-v0-factory-v-99362_>`_

.. _type-daml-finance-interface-instrument-equity-v0-factory-view-61438:

**data** `View <type-daml-finance-interface-instrument-equity-v0-factory-view-61438_>`_

  .. _constr-daml-finance-interface-instrument-equity-v0-factory-view-62775:

  `View <constr-daml-finance-interface-instrument-equity-v0-factory-view-62775_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-equity-v0-factory-view-61438_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-equity-v0-factory-view-61438_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-equity-v0-factory-createtick-23133:

`create' <function-daml-finance-interface-instrument-equity-v0-factory-createtick-23133_>`_
  \: `Factory <type-daml-finance-interface-instrument-equity-v0-factory-factory-21456_>`_ \-\> `Create <type-daml-finance-interface-instrument-equity-v0-factory-create-45111_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-equity-v0-instrument-i-47875>`)
