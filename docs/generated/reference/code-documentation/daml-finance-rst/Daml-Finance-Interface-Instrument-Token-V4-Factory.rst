.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-token-v4-factory-1398:

Daml.Finance.Interface.Instrument.Token.V4.Factory
==================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-token-v4-factory-factory-54831:

**interface** `Factory <type-daml-finance-interface-instrument-token-v4-factory-factory-54831_>`_

  Factory interface to instantiate simple tokens\.

  **viewtype** `V <type-daml-finance-interface-instrument-token-v4-factory-v-36005_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-token-v4-factory-create-20178:

    **Choice** `Create <type-daml-finance-interface-instrument-token-v4-factory-create-20178_>`_

    Create a new token\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" token)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" token))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-token-v4-instrument-i-45050>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - token
         - :ref:`Token <type-daml-finance-interface-instrument-token-v4-types-token-51711>`
         - Attributes to create a token\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-token-v4-factory-create-20178_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-token-v4-instrument-i-45050>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-token-v4-factory-i-46898:

**type** `I <type-daml-finance-interface-instrument-token-v4-factory-i-46898_>`_
  \= `Factory <type-daml-finance-interface-instrument-token-v4-factory-factory-54831_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-token-v4-factory-v-36005:

**type** `V <type-daml-finance-interface-instrument-token-v4-factory-v-36005_>`_
  \= `View <type-daml-finance-interface-instrument-token-v4-factory-view-14603_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-token-v4-factory-factory-54831_>`_ `V <type-daml-finance-interface-instrument-token-v4-factory-v-36005_>`_

.. _type-daml-finance-interface-instrument-token-v4-factory-view-14603:

**data** `View <type-daml-finance-interface-instrument-token-v4-factory-view-14603_>`_

  .. _constr-daml-finance-interface-instrument-token-v4-factory-view-27112:

  `View <constr-daml-finance-interface-instrument-token-v4-factory-view-27112_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-token-v4-factory-view-14603_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-token-v4-factory-view-14603_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-token-v4-factory-createtick-64546:

`create' <function-daml-finance-interface-instrument-token-v4-factory-createtick-64546_>`_
  \: `Factory <type-daml-finance-interface-instrument-token-v4-factory-factory-54831_>`_ \-\> `Create <type-daml-finance-interface-instrument-token-v4-factory-create-20178_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-token-v4-instrument-i-45050>`)
