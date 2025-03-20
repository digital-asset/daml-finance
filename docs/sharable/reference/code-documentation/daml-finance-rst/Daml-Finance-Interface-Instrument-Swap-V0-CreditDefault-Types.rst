.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-creditdefault-types-7878:

Daml.Finance.Interface.Instrument.Swap.V0.CreditDefault.Types
=============================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509:

**data** `CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509_>`_

  Describes the attributes of a Credit Default swap\.

  .. _constr-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-31326:

  `CreditDefault <constr-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-31326_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The instrument's key\.
       * - description
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The description of the swap\.
       * - defaultProbabilityReferenceId
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The reference ID of the default probability observable\. For example, in case of protection against a \"TSLA bond payment default\" this should be a valid reference to the \"TSLA default probability\"\.
       * - recoveryRateReferenceId
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The reference ID of the recovery rate observable\. For example, in case of a \"TSLA bond payment default with a 60% recovery rate\" this should be a valid reference to the \"TSLA bond recovery rate\"\.
       * - ownerReceivesFix
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - Indicate whether a holding owner of this instrument receives the fix or the default protection leg of the swap\.
       * - fixRate
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The interest rate of the fix leg\. For example, in case of \"2\.5% fix\" this should be 0\.025\.
       * - periodicSchedule
         - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
         - The schedule for the periodic swap payments\.
       * - holidayCalendarIds
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         - The identifiers of the holiday calendars to be used for the swap payment schedule\.
       * - calendarDataProvider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The reference data provider to use for the holiday calendar\.
       * - dayCountConvention
         - :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
         - The day count convention used to calculate day count fractions\. For example\: Act360\.
       * - currency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The currency of the swap\. For example, if the swap pays in USD this should be a USD cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `CreditDefault <type-daml-finance-interface-instrument-swap-v0-creditdefault-types-creditdefault-30509_>`_
