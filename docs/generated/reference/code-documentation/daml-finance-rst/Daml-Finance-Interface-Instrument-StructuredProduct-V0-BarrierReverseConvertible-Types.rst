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
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The description of the option\.
       * - referenceAssetId
         - `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The reference asset ID\. For example, in case of an option on AAPL this should be a valid reference to the AAPL fixings to be used for the payoff calculation\.
       * - strike
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The strike price of the option\.
       * - barrier
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The barrier level of the option\.
       * - barrierStartDate
         - `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The start date for barrier observations\.
       * - expiryDate
         - `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The expiry date of the instrument\.
       * - couponRate
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fixed coupon rate, per annum\. For example, in case of a \"3\.5% p\.a coupon\" this should be 0\.035\.
       * - periodicSchedule
         - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
         - The schedule for the periodic coupon payments\.
       * - holidayCalendarIds
         - \[`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         - The identifiers of the holiday calendars to be used for the coupon schedule\.
       * - calendarDataProvider
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The reference data provider to use for the holiday calendar\.
       * - dayCountConvention
         - :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
         - The day count convention used to calculate day count fractions\. For example\: Act360\.
       * - currency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The currency of the product\. For example, if the product pays in USD this should be a USD cash instrument\.
       * - notional
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The notional of the product\. This is the face value corresponding to one unit of the product\. For example, if one product unit corresponds to 1000 USD, this should be 1000\.0\.
       * - lastEventTimestamp
         - `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
       * - prevEvents
         - \[EventData\]
         - A list of previous events that have been lifecycled on this instrument so far\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrier\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrierReverseConvertible\" :ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-create-85905>` `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrierReverseConvertible\" :ref:`View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-view-1802>` `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"barrierStartDate\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"calendarDataProvider\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"couponRate\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"description\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"expiryDate\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holidayCalendarIds\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ \[`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lastEventTimestamp\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"notional\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"periodicSchedule\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"prevEvents\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ \[EventData\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"referenceAssetId\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"strike\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrier\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrierReverseConvertible\" :ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-factory-create-85905>` `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrierReverseConvertible\" :ref:`View <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-view-1802>` `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"barrierStartDate\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"calendarDataProvider\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"couponRate\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"description\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"expiryDate\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Date <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holidayCalendarIds\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ \[`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lastEventTimestamp\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"notional\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"periodicSchedule\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"prevEvents\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ \[EventData\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"referenceAssetId\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"strike\" `BarrierReverseConvertible <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-types-barrierreverseconvertible-74687_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
