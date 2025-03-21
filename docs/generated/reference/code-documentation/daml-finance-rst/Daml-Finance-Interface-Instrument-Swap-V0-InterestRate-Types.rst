.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-interestrate-types-74433:

Daml.Finance.Interface.Instrument.Swap.V0.InterestRate.Types
============================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655:

**data** `InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655_>`_

  .. _constr-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-50540:

  `InterestRate <constr-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-50540_>`_

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
       * - floatingRate
         - :ref:`FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149>`
         - A description of the floating rate to be used\. This supports both Libor and SOFR style reference rates (using a compounded index, e\.g\. the SOFR Index)\.
       * - ownerReceivesFix
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - Indicate whether a holding owner of this instrument receives the fix or the floating leg of the swap\.
       * - fixRate
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The interest rate of the fix leg\. For example, in case of \"3M Euribor vs 2\.5% fix\" this should be 0\.025\.
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

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `InterestRate <type-daml-finance-interface-instrument-swap-v0-interestrate-types-interestrate-17655_>`_
