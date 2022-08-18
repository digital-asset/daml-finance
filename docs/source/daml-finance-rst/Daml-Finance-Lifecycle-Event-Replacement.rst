.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-event-replacement-51859:

Module Daml.Finance.Lifecycle.Event.Replacement
===============================================

Templates
---------

.. _type-daml-finance-lifecycle-event-replacement-event-16120:

**template** `Event <type-daml-finance-lifecycle-event-replacement-event-16120_>`_

  Replacement of units of an instrument with a basket of other instruments\.
  
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
       - A textual identifier of the event\.
     * - effectiveDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - Date on which the replacement is effectuated\.
     * - targetInstrument
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - Instrument the replacement event applies to\.
     * - perUnitReplacement
       - \[:ref:`Q <type-daml-finance-interface-asset-instrument-q-31714>`\]
       - Instrument quantities the target instrument is replaced with\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
  
  + **Choice Archive**
    

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>`

Data Types
----------

.. _type-daml-finance-lifecycle-event-replacement-t-85632:

**type** `T <type-daml-finance-lifecycle-event-replacement-t-85632_>`_
  \= `Event <type-daml-finance-lifecycle-event-replacement-event-16120_>`_
