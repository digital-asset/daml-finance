.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-currency-instrument-96013:

Daml.Finance.Interface.Instrument.Swap.V0.Currency.Instrument
=============================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-swap-v0-currency-instrument-instrument-88662:

**interface** `Instrument <type-daml-finance-interface-instrument-swap-v0-currency-instrument-instrument-88662_>`_

  Instrument interface representing a currency swap\.

  **viewtype** `V <type-daml-finance-interface-instrument-swap-v0-currency-instrument-v-87776_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-instrument-swap-v0-currency-instrument-getview-82375:

    **Choice** `GetView <type-daml-finance-interface-instrument-swap-v0-currency-instrument-getview-82375_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `V <type-daml-finance-interface-instrument-swap-v0-currency-instrument-v-87776_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party retrieving the view\.


Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-currency-instrument-i-94263:

**type** `I <type-daml-finance-interface-instrument-swap-v0-currency-instrument-i-94263_>`_
  \= `Instrument <type-daml-finance-interface-instrument-swap-v0-currency-instrument-instrument-88662_>`_

  Type synonym for ``Instrument``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-instrument-swap-v0-currency-factory-factory-57464>` \"create'\" (:ref:`Create <type-daml-finance-interface-instrument-swap-v0-currency-factory-create-28047>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-instrument-swap-v0-currency-instrument-i-94263_>`_))

.. _type-daml-finance-interface-instrument-swap-v0-currency-instrument-v-87776:

**type** `V <type-daml-finance-interface-instrument-swap-v0-currency-instrument-v-87776_>`_
  \= `View <type-daml-finance-interface-instrument-swap-v0-currency-instrument-view-10332_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Instrument <type-daml-finance-interface-instrument-swap-v0-currency-instrument-instrument-88662_>`_ `V <type-daml-finance-interface-instrument-swap-v0-currency-instrument-v-87776_>`_

.. _type-daml-finance-interface-instrument-swap-v0-currency-instrument-view-10332:

**data** `View <type-daml-finance-interface-instrument-swap-v0-currency-instrument-view-10332_>`_

  View of ``Instrument``\.

  .. _constr-daml-finance-interface-instrument-swap-v0-currency-instrument-view-2849:

  `View <constr-daml-finance-interface-instrument-swap-v0-currency-instrument-view-2849_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - currencySwap
         - :ref:`CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660>`
         - Attributes of a currency swap\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-swap-v0-currency-instrument-view-10332_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-swap-v0-currency-instrument-view-10332_>`_
