.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-swap-v0-fpml-factory-61592:

Daml.Finance.Instrument.Swap.V0.Fpml.Factory
============================================

Templates
---------

.. _type-daml-finance-instrument-swap-v0-fpml-factory-factory-13237:

**template** `Factory <type-daml-finance-instrument-swap-v0-fpml-factory-factory-13237_>`_

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

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-swap-v0-fpml-factory-i-73357>` **for** `Factory <type-daml-finance-instrument-swap-v0-fpml-factory-factory-13237_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-instrument-swap-v0-fpml-factory-factory-13237_>`_

Data Types
----------

.. _type-daml-finance-instrument-swap-v0-fpml-factory-t-69693:

**type** `T <type-daml-finance-instrument-swap-v0-fpml-factory-t-69693_>`_
  \= `Factory <type-daml-finance-instrument-swap-v0-fpml-factory-factory-13237_>`_

  Type synonym for ``Factory``\.
