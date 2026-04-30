.. Copyright (c) 2026 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-currency-types-12994:

Daml.Finance.Interface.Instrument.Swap.V0.Currency.Types
========================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660:

**data** `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_

  Describes the attributes of a Currency swap\.

  .. _constr-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-97991:

  `CurrencySwap <constr-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-97991_>`_

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
         - The description of the swap\.
       * - ownerReceivesBase
         - `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - Indicate whether a holding owner of this instrument receives the base currency leg or the foreign currency leg of the swap\.
       * - baseRate
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The interest rate of the base currency\. For example, in case of \"3% in USD\" this should be 0\.03\.
       * - foreignRate
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The interest rate of the foreign currency\. For example, in case of \"2% in EUR\" this should be 0\.02\.
       * - periodicSchedule
         - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
         - The schedule for the periodic swap payments\.
       * - holidayCalendarIds
         - \[`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         - The identifiers of the holiday calendars to be used for the swap payment schedule\.
       * - calendarDataProvider
         - `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The reference data provider to use for the holiday calendar\.
       * - dayCountConvention
         - :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
         - The day count convention used to calculate day count fractions\. For example\: Act360\.
       * - baseCurrency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The base currency of the swap\. For example, in the case of USD this should be a USD cash instrument\.
       * - foreignCurrency
         - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
         - The foreign currency of the swap\. For example, in case of EUR this should be a EUR cash instrument\.
       * - fxRate
         - `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fx rate used to convert from the base currency principal amount to the foreign currency principal amount\.
       * - lastEventTimestamp
         - `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  **instance** `Eq <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_

  **instance** `Show <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"baseCurrency\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"baseRate\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"calendarDataProvider\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currencySwap\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-currency-factory-create-28047>` `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"currencySwap\" :ref:`View <type-daml-finance-interface-instrument-swap-v0-currency-instrument-view-10332>` `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"dayCountConvention\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"description\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"foreignCurrency\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"foreignRate\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"fxRate\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"holidayCalendarIds\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ \[`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"instrument\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"lastEventTimestamp\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"ownerReceivesBase\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  **instance** `GetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-getfield-53979>`_ \"periodicSchedule\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"baseCurrency\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"baseRate\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"calendarDataProvider\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Party <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currencySwap\" :ref:`Create <type-daml-finance-interface-instrument-swap-v0-currency-factory-create-28047>` `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"currencySwap\" :ref:`View <type-daml-finance-interface-instrument-swap-v0-currency-instrument-view-10332>` `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"dayCountConvention\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"description\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"foreignCurrency\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"foreignRate\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"fxRate\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Decimal <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"holidayCalendarIds\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ \[`Text <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"instrument\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"lastEventTimestamp\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Time <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"ownerReceivesBase\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ `Bool <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  **instance** `SetField <https://docs.digitalasset.com/build/3.4/reference/daml/stdlib/DA-Record.html#class-da-internal-record-setfield-4311>`_ \"periodicSchedule\" `CurrencySwap <type-daml-finance-interface-instrument-swap-v0-currency-types-currencyswap-39660_>`_ :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
