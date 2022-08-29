.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-event-distribution-17302:

Module Daml.Finance.Lifecycle.Event.Distribution
================================================

Templates
---------

.. _type-daml-finance-lifecycle-event-distribution-event-46459:

**template** `Event <type-daml-finance-lifecycle-event-distribution-event-46459_>`_

  Distribution of units of an instrument for each unit of a target instrument (e\.g\., share or cash dividends)\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - Providers of the distribution event\.
     * - id
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Textual identifier of the event\.
     * - effectiveDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - Date on which the distribution is effectuated\.
     * - targetInstrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - Instrument the distribution event applies to\.
     * - newInstrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - Instrument after the distribution has been claimed\.
     * - perUnitDistribution
       - \[:ref:`Q <type-daml-finance-interface-asset-instrument-q-31714>`\]
       - Distributed quantities per unit held\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

Data Types
----------

.. _type-daml-finance-lifecycle-event-distribution-t-61859:

**type** `T <type-daml-finance-lifecycle-event-distribution-t-61859_>`_
  \= `Event <type-daml-finance-lifecycle-event-distribution-event-46459_>`_
