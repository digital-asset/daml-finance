.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-creditdefault-factory-8179:

Daml.Finance.Interface.Instrument.Swap.V0.CreditDefault.Factory
===============================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-factory-36744:

**interface** `Factory <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-factory-36744_>`_

  Factory interface to instantiate credit default swaps\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-v-76602_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-create-56287:

    **Choice** `Create <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-create-56287_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" creditDefault)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" creditDefault))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-i-43747>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - creditDefault
         - :ref:`CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509>`
         - Attributes to create a credit default swap\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-create-56287_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-i-43747>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-i-74269:

**type** `I <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-i-74269_>`_
  \= `Factory <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-factory-36744_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-v-76602:

**type** `V <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-v-76602_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-view-77110_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-factory-36744_>`_ `V <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-v-76602_>`_

.. _type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-view-77110:

**data** `View <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-view-77110_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-creditdefault-factory-view-52339:

  `View <constr-daml-finance-interface-instrument-swap-v0-creditdefault-factory-view-52339_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-view-77110_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-view-77110_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-swap-v0-creditdefault-factory-createtick-17293:

`create' <function-daml-finance-interface-instrument-swap-v0-creditdefault-factory-createtick-17293_>`_
  \: `Factory <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-factory-36744_>`_ \-\> `Create <type-daml-finance-interface-instrument-swap-v0-creditdefault-factory-create-56287_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-i-43747>`)
