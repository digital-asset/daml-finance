.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-90803:

Daml.Finance.Interface.Instrument.Bond.V3.InflationLinked.Factory
=================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-factory-33440:

**interface** `Factory <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-factory-33440_>`_

  Factory interface to instantiate inflation\-linked bond instruments\.

  **viewtype** `V <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-v-4818_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-create-17927:

    **Choice** `Create <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-create-17927_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" inflationLinked)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" inflationLinked))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-i-26275>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - inflationLinked
         - :ref:`InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736>`
         - Attributes to create an inflation linked bond\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-create-17927_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-i-26275>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-i-93925:

**type** `I <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-i-93925_>`_
  \= `Factory <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-factory-33440_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-v-4818:

**type** `V <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-v-4818_>`_
  \= `View <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-view-20750_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-factory-33440_>`_ `V <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-v-4818_>`_

.. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-view-20750:

**data** `View <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-view-20750_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-view-98487:

  `View <constr-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-view-98487_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-view-20750_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-view-20750_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-createtick-10725:

`create' <function-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-createtick-10725_>`_
  \: `Factory <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-factory-33440_>`_ \-\> `Create <type-daml-finance-interface-instrument-bond-v3-inflationlinked-factory-create-17927_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-i-26275>`)
