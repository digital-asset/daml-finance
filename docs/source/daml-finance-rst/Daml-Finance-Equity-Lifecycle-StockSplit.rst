.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-equity-lifecycle-stocksplit-2384:

Module Daml.Finance.Equity.Lifecycle.StockSplit
===============================================

Templates
---------

.. _type-daml-finance-equity-lifecycle-stocksplit-stocksplit-51182:

**template** `StockSplit <type-daml-finance-equity-lifecycle-stocksplit-stocksplit-51182_>`_

  Stock split\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Issuer of the Instrument
     * - currentInstrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - Equity impacted by the stock split
     * - newInstrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - Equity to hold after the stock split
     * - factor
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - Factor to increase/decrease the total stock for an instrument
     * - effectiveDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - Date the stock split occurs
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Id for this event
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-equity-lifecycle-corporateaction-i-47005>`
  
  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`
  
  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

Data Types
----------

.. _type-daml-finance-equity-lifecycle-stocksplit-t-63365:

**type** `T <type-daml-finance-equity-lifecycle-stocksplit-t-63365_>`_
  \= `StockSplit <type-daml-finance-equity-lifecycle-stocksplit-stocksplit-51182_>`_
  
  **instance** :ref:`HasImplementation <class-daml-finance-interface-lifecycle-lifecyclable-hasimplementation-23622>` `T <type-daml-finance-equity-lifecycle-stocksplit-t-63365_>`_
