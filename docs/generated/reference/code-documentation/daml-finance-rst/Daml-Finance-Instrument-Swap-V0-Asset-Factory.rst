.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-swap-v0-asset-factory-78880:

Daml.Finance.Instrument.Swap.V0.Asset.Factory
=============================================

Templates
---------

.. _type-daml-finance-instrument-swap-v0-asset-factory-factory-64565:

**template** `Factory <type-daml-finance-instrument-swap-v0-asset-factory-factory-64565_>`_

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

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-swap-v0-asset-factory-i-5355>` **for** `Factory <type-daml-finance-instrument-swap-v0-asset-factory-factory-64565_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-instrument-swap-v0-asset-factory-factory-64565_>`_

Data Types
----------

.. _type-daml-finance-instrument-swap-v0-asset-factory-t-86685:

**type** `T <type-daml-finance-instrument-swap-v0-asset-factory-t-86685_>`_
  \= `Factory <type-daml-finance-instrument-swap-v0-asset-factory-factory-64565_>`_

  Type synonym for ``Factory``\.
