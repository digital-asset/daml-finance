.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-swap-v0-currency-factory-5168:

Daml.Finance.Instrument.Swap.V0.Currency.Factory
================================================

Templates
---------

.. _type-daml-finance-instrument-swap-v0-currency-factory-factory-69341:

**template** `Factory <type-daml-finance-instrument-swap-v0-currency-factory-factory-69341_>`_

  Factory template for instrument creation\.

  Signatory\: provider

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The factory's provider\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The factory's observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-swap-v0-currency-factory-i-59021>` **for** `Factory <type-daml-finance-instrument-swap-v0-currency-factory-factory-69341_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-instrument-swap-v0-currency-factory-factory-69341_>`_

Data Types
----------

.. _type-daml-finance-instrument-swap-v0-currency-factory-t-74629:

**type** `T <type-daml-finance-instrument-swap-v0-currency-factory-t-74629_>`_
  \= `Factory <type-daml-finance-instrument-swap-v0-currency-factory-factory-69341_>`_

  Type synonym for ``Factory``\.
