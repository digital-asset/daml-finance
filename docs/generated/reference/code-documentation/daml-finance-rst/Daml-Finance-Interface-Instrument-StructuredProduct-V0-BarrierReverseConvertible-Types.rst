.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-60320:

Daml.Finance.Interface.Instrument.StructuredProduct.V0.BarrierReverseConvertible.Types
======================================================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687:

**data** `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_

  Describes the attributes of a Barrier Reverse Convertible (BRC) instrument\.
  It can be seen as a long fixed coupon bond and a short Down\-And\-In put option\.

  .. _constr-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-85126:

  `BarrierReverseConvertible <constr-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-85126_>`_

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
         - The description of the option\.
       * - referenceAssetId
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The reference asset ID\. For example, in case of an option on AAPL this should be a valid reference to the AAPL fixings to be used for the payoff calculation\.
       * - strike
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The strike price of the option\.
       * - barrier
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The barrier level of the option\.
       * - barrierStartDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The start date for barrier observations\.
       * - expiryDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The expiry date of the instrument\.
       * - couponRate
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fixed coupon rate, per annum\. For example, in case of a \"3\.5% p\.a coupon\" this should be 0\.035\.
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
         - The currency of the product\. For example, if the product pays in USD this should be a USD cash instrument\.
       * - notional
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The notional of the product\. This is the face value corresponding to one unit of the product\. For example, if one product unit corresponds to 1000 USD, this should be 1000\.0\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
       * - prevEvents
         - \[EventData\]
         - A list of previous events that have been lifecycled on this instrument so far\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_
