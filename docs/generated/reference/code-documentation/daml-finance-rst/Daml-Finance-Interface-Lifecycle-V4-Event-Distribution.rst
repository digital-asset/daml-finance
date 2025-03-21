.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-lifecycle-v4-event-distribution-56030:

Daml.Finance.Interface.Lifecycle.V4.Event.Distribution
======================================================

Interfaces
----------

.. _type-daml-finance-interface-lifecycle-v4-event-distribution-event-40183:

**interface** `Event <type-daml-finance-interface-lifecycle-v4-event-distribution-event-40183_>`_

  Event interface for the distribution of units of an instrument for each unit of a target
  instrument (e\.g\. share or cash dividends)\.

  **viewtype** `V <type-daml-finance-interface-lifecycle-v4-event-distribution-v-87033_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-lifecycle-v4-event-distribution-getview-4518:

    **Choice** `GetView <type-daml-finance-interface-lifecycle-v4-event-distribution-getview-4518_>`_

    Retrieves the event view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671_>`_

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

.. _type-daml-finance-interface-lifecycle-v4-event-distribution-i-24126:

**type** `I <type-daml-finance-interface-lifecycle-v4-event-distribution-i-24126_>`_
  \= `Event <type-daml-finance-interface-lifecycle-v4-event-distribution-event-40183_>`_

  Type synonym for ``Event``\.

.. _type-daml-finance-interface-lifecycle-v4-event-distribution-v-87033:

**type** `V <type-daml-finance-interface-lifecycle-v4-event-distribution-v-87033_>`_
  \= `View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Event <type-daml-finance-interface-lifecycle-v4-event-distribution-event-40183_>`_ `V <type-daml-finance-interface-lifecycle-v4-event-distribution-v-87033_>`_

.. _type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671:

**data** `View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671_>`_

  View for ``Event``\.

  .. _constr-daml-finance-interface-lifecycle-v4-event-distribution-view-5964:

  `View <constr-daml-finance-interface-lifecycle-v4-event-distribution-view-5964_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
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

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-lifecycle-v4-event-distribution-view-42671_>`_
