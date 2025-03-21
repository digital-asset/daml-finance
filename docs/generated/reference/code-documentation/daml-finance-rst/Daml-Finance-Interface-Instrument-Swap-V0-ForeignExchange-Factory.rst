.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-72042:

Daml.Finance.Interface.Instrument.Swap.V0.ForeignExchange.Factory
=================================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-factory-72691:

**interface** `Factory <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-factory-72691_>`_

  Factory interface to instantiate foreign exchange swaps\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-v-25921_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-create-93054:

    **Choice** `Create <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-create-93054_>`_

    Create a new instrument\.

    Controller\: (DA\.Internal\.Record\.getField @\"depository\" (DA\.Internal\.Record\.getField @\"instrument\" foreignExchange)), (DA\.Internal\.Record\.getField @\"issuer\" (DA\.Internal\.Record\.getField @\"instrument\" foreignExchange))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-i-60062>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - foreignExchange
         - :ref:`ForeignExchange <type-daml-finance-interface-instrument-swap-v0-foreignexchange-types-foreignexchange-59609>`
         - Attributes to create an FX swap\.
       * - observers
         - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
         - The instrument's observers\.

  + **Method create' \:** `Create <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-create-93054_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-i-60062>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-i-93494:

**type** `I <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-i-93494_>`_
  \= `Factory <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-factory-72691_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-v-25921:

**type** `V <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-v-25921_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-view-9847_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-factory-72691_>`_ `V <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-v-25921_>`_

.. _type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-view-9847:

**data** `View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-view-9847_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-view-1938:

  `View <constr-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-view-1938_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-view-9847_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-view-9847_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-createtick-98726:

`create' <function-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-createtick-98726_>`_
  \: `Factory <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-factory-72691_>`_ \-\> `Create <type-daml-finance-interface-instrument-swap-v0-foreignexchange-factory-create-93054_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-swap-v0-foreignexchange-instrument-i-60062>`)
