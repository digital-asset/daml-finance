.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-v3-inflationlinked-types-43490:

Daml.Finance.Interface.Instrument.Bond.V3.InflationLinked.Types
===============================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736:

**data** `InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736_>`_

  Describes the attributes of an Inflation Linked Bond\.

  .. _constr-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-90435:

  `InflationLinked <constr-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-90435_>`_

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
         - The description of the bond\.
       * - inflationIndexId
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The inflation index reference ID\. For example, in case of \"0\.5% p\.a\. coupon, CPI adjusted principal\" this should be a valid reference to the \"CPI\" index\.
       * - inflationIndexBaseValue
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The value of the inflation index on the first reference date of this bond (called \"dated date\" on US TIPS)\. This is used as the base value for the principal adjustment\.
       * - couponRate
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fixed coupon rate, per annum\. For example, in case of a \"0\.5% p\.a\. coupon, CPI adjusted principal\" this should be 0\.005\.
       * - periodicSchedule
         - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
         - The schedule for the periodic coupon payments\.
       * - holidayCalendarIds
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         - The identifiers of the holiday calendars to be used for the coupon schedule\.
       * - calendarDataProvider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The reference data provider to use for the holiday calendar\.
       * - dayCountConvention
         - :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
         - The day count convention used to calculate day count fractions\. For example\: Act360\.
       * - currency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The currency of the bond\. For example, if the bond pays in USD this should be a USD cash instrument\.
       * - notional
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The notional of the bond\. This is the face value corresponding to one unit of the bond instrument\. For example, if one bond unit corresponds to 1000 USD, this should be 1000\.0\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `InflationLinked <type-daml-finance-interface-instrument-bond-v3-inflationlinked-types-inflationlinked-43736_>`_
