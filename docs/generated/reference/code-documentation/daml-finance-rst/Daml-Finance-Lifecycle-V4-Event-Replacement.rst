.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-lifecycle-v4-event-replacement-94706:

Daml.Finance.Lifecycle.V4.Event.Replacement
===========================================

Templates
---------

.. _type-daml-finance-lifecycle-v4-event-replacement-event-94835:

**template** `Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835_>`_

  Event contract for the replacement of units of an instrument with a basket of other
  instruments, e\.g\., a stock merger\.

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
       - Event identifier\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - Event description\.
     * - effectiveTime
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - Time on which the replacement is effectuated\.
     * - targetInstrument
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - Instrument the replacement event applies to\.
     * - perUnitReplacement
       - \[:ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>`\]
       - Instrument quantities the target instrument is replaced with\.
     * - observers
       - :ref:`Parties <type-daml-finance-interface-types-common-v3-types-parties-67059>`
       - Observers\.

  + **Choice** Archive

    Controller\: providers

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-event-i-36171>` **for** `Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-lifecycle-v4-event-replacement-i-69673>` **for** `Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835_>`_

Data Types
----------

.. _type-daml-finance-lifecycle-v4-event-replacement-t-18539:

**type** `T <type-daml-finance-lifecycle-v4-event-replacement-t-18539_>`_
  \= `Event <type-daml-finance-lifecycle-v4-event-replacement-event-94835_>`_

  Type synonym for ``Event``\.
