.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-v4-event-distribution-38493:

Daml.Finance.Lifecycle.V4.Event.Distribution
============================================

Templates
---------

.. _type-daml-finance-lifecycle-v4-event-distribution-event-43030:

**template** `Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030_>`_

  Event contract for the distribution of units of an instrument for each unit of a target
  instrument (e\.g\. share or cash dividends)\.

  Signatory\: providers

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - providers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Providers of the distribution event\.
     * - id
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - Event Identifier\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Event description\.
     * - effectiveTime
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Time on which the distribution is effectuated\.
     * - targetInstrument
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - Instrument the distribution event applies to\.
     * - newInstrument
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - Instrument after the distribution has been claimed\.
     * - perUnitDistribution
       - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
       - Distributed quantities per unit held\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Observers\.

  + **Choice** Archive

    Controller\: providers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-event-distribution-i-24126>` **for** `Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>` **for** `Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030_>`_

Data Types
----------

.. _type-daml-finance-lifecycle-v4-event-distribution-t-75422:

**type** `T <type-daml-finance-lifecycle-v4-event-distribution-t-75422_>`_
  \= `Event <type-daml-finance-lifecycle-v4-event-distribution-event-43030_>`_

  Type synonym for ``Event``\.
