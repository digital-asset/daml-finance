.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-24024:

Daml.Finance.Interface.Instrument.StructuredProduct.V0.AutoCallable.Types
=========================================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435:

**data** `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_

  Describes the attributes of an AutoCallable instrument that pays a conditional coupon\.

  .. _constr-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-34750:

  `AutoCallable <constr-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-34750_>`_

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
         - The reference asset ID\. For example, in case of an AAPL underlying this should be a valid reference to the AAPL fixings to be used for the payoff calculation\.
       * - putStrike
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The strike of the put (as a percentage of the underlying closing price on the first observation date)\.
       * - couponBarrier
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The coupon barrier (as a percentage of the underlying closing price on the first observation date)\.
       * - callBarrier
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The barrier used to automatically call the instrument (as a percentage of the underlying closing price on the first observation date)\.
       * - finalBarrier
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The barrier used to determine the final redemption amount (as a percentage of the underlying closing price on the first observation date)\.
       * - couponRate
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fixed coupon rate, either per annum or per coupon period (depending on the dayCountConvention below)\.
       * - observationSchedule
         - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
         - The schedule for the observation dates\. These are used to observe the barrier, determine whether the instrument is automatically called and to determine the final redemption amount\.
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

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"autoCallable\" :ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-create-58961>` `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"autoCallable\" :ref:`View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-view-56578>` `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"calendarDataProvider\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"callBarrier\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"couponBarrier\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"couponRate\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currency\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"description\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"finalBarrier\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holidayCalendarIds\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ \[`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lastEventTimestamp\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"notional\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"observationSchedule\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"periodicSchedule\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"prevEvents\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ \[EventData\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"putStrike\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"referenceAssetId\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"autoCallable\" :ref:`Create <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-factory-create-58961>` `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"autoCallable\" :ref:`View <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-instrument-view-56578>` `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"calendarDataProvider\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"callBarrier\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"couponBarrier\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"couponRate\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currency\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"description\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"finalBarrier\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holidayCalendarIds\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ \[`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lastEventTimestamp\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"notional\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"observationSchedule\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"periodicSchedule\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"prevEvents\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ \[EventData\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"putStrike\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"referenceAssetId\" `AutoCallable <type-daml-finance-interface-instrument-structuredproduct-v0-autocallable-types-autocallable-58435_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
