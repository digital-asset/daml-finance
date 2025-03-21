.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-14103:

Daml.Finance.Interface.Instrument.Swap.V0.Fpml.FpmlTypes
========================================================

Data Types
----------

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-adjustabledate-37102:

**data** `AdjustableDate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-adjustabledate-37102_>`_

  A type for defining a date that shall be subject to adjustment if it would otherwise fall on a
  day that is not a business day in the specified business centers, together with the convention
  for adjusting the date\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-adjustabledate-88773:

  `AdjustableDate <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-adjustabledate-88773_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - unadjustedDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - A date subject to adjustment\.
       * - dateAdjustments
         - `BusinessDayAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-9111_>`_
         - The business day convention and financial business centers used for adjusting the date if it would otherwise fall on a day that is not a business date in the specified business centers\. adjustedDate \: Optional IdentifiedDate \^ The date once the adjustment has been performed\. (Note that this date may change if the business center holidays change)\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `AdjustableDate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-adjustabledate-37102_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `AdjustableDate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-adjustabledate-37102_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businesscentertime-84241:

**data** `BusinessCenterTime <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businesscentertime-84241_>`_

  A type for defining a time with respect to a business day calendar location\. For example,
  11\:00am London time\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businesscentertime-76210:

  `BusinessCenterTime <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businesscentertime-76210_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - hourMinuteTime
         - `HourMinuteTime <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-hourminutetime-47030_>`_
         - A time specified in hh\:mm\:ss format where the second component must be '00', e\.g\., 11am would be represented as 11\:00\:00\.
       * - businessCenter
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BusinessCenterTime <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businesscentertime-84241_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BusinessCenterTime <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businesscentertime-84241_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-9111:

**data** `BusinessDayAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-9111_>`_

  A type defining the business day convention and financial business centers used for adjusting
  any relevant date if it would otherwise fall on a day that is not a business day in the
  specified business centers\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-36204:

  `BusinessDayAdjustments <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-36204_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - businessDayConvention
         - :ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>`
         - The convention for adjusting a date if it would otherwise fall on a day that is not a business day\.
       * - businessCenters
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BusinessDayAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-9111_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BusinessDayAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-9111_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculation-57533:

**data** `Calculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculation-57533_>`_

  The parameters used in the calculation of fixed or floating rate period amounts\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculation-37464:

  `Calculation <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculation-37464_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - notionalScheduleValue
         - `NotionalScheduleValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedulevalue-85970_>`_
         -
       * - rateTypeValue
         - `RateTypeValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-ratetypevalue-70501_>`_
         -
       * - dayCountFraction
         - :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
         -
       * - compoundingMethodEnum
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `CompoundingMethodEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-compoundingmethodenum-42513_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Calculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculation-57533_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Calculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculation-57533_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodamount-86226:

**data** `CalculationPeriodAmount <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodamount-86226_>`_

  The calculation period amount parameters\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodamount-61171:

  `CalculationPeriodAmount <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodamount-61171_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - calculation
         - `Calculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculation-57533_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `CalculationPeriodAmount <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodamount-86226_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `CalculationPeriodAmount <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodamount-86226_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddates-23760:

**data** `CalculationPeriodDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddates-23760_>`_

  The calculation periods dates schedule\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddates-88403:

  `CalculationPeriodDates <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddates-88403_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - effectiveDate
         - `AdjustableDate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-adjustabledate-37102_>`_
         -
       * - terminationDate
         - `AdjustableDate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-adjustabledate-37102_>`_
         -
       * - calculationPeriodDatesAdjustments
         - `CalculationPeriodDatesAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddatesadjustments-85977_>`_
         -
       * - firstPeriodStartDate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `AdjustableDate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-adjustabledate-37102_>`_
         -
       * - firstRegularPeriodStartDate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         -
       * - lastRegularPeriodEndDate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         -
       * - calculationPeriodFrequency
         - `CalculationPeriodFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodfrequency-78985_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `CalculationPeriodDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddates-23760_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `CalculationPeriodDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddates-23760_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddatesadjustments-85977:

**data** `CalculationPeriodDatesAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddatesadjustments-85977_>`_

  The business day convention to apply to each calculation period end date if it would otherwise
  fall on a day that is not a business day in the specified financial business centers\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddatesadjustments-43236:

  `CalculationPeriodDatesAdjustments <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddatesadjustments-43236_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - businessDayConvention
         - :ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>`
         -
       * - businessCenters
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `CalculationPeriodDatesAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddatesadjustments-85977_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `CalculationPeriodDatesAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddatesadjustments-85977_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodfrequency-78985:

**data** `CalculationPeriodFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodfrequency-78985_>`_

  A type defining the frequency at which calculation period end dates occur within the regular
  part of the calculation period schedule and thier roll date convention\. In case the calculation
  frequency is of value T (term), the period is defined by the
  swap\\swapStream\\calculationPerioDates\\effectiveDate and the
  swap\\swapStream\\calculationPerioDates\\terminationDate\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodfrequency-31442:

  `CalculationPeriodFrequency <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodfrequency-31442_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - periodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         - A time period multiplier, e\.g\., 1, 2 or 3 etc\. If the period value is T (Term) then periodMultiplier must contain the value 1\.
       * - period
         - `PeriodExtendedEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-periodextendedenum-38896_>`_
         - A time period, e\.g\., a day, week, month, year or term of the stream\.
       * - rollConvention
         - :ref:`RollConventionEnum <type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490>`
         - Used in conjunction with a frequency and the regular period start date of a calculation period, determines each calculation period end date within the regular part of a c alculation period schedule\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `CalculationPeriodFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodfrequency-78985_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `CalculationPeriodFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodfrequency-78985_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-compoundingmethodenum-42513:

**data** `CompoundingMethodEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-compoundingmethodenum-42513_>`_

  The compounding calculation method

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-flat-18311:

  `Flat <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-flat-18311_>`_

    Flat compounding\. Compounding excludes the spread\. Note that the first compounding period
    has it's interest calculated including any spread then subsequent periods compound this at a
    rate excluding the spread\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-nocompounding-1947:

  `NoCompounding <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-nocompounding-1947_>`_

    No compounding is to be applied\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-straight-53304:

  `Straight <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-straight-53304_>`_

    Straight compounding\. Compounding includes the spread\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-spreadexclusive-98588:

  `SpreadExclusive <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-spreadexclusive-98588_>`_

    Spread Exclusive compounding\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `CompoundingMethodEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-compoundingmethodenum-42513_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `CompoundingMethodEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-compoundingmethodenum-42513_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-dateoffset-80142:

**data** `DateOffset <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-dateoffset-80142_>`_

  A type defining an offset used in calculating a date when this date is defined in reference to
  another date through a date offset\. The type includes the convention for adjusting the date and
  an optional sequence element to indicate the order in a sequence of multiple date offsets\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-dateoffset-62325:

  `DateOffset <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-dateoffset-62325_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - periodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         - A time period multiplier, e\.g\. 1, 2 or 3 etc\. A negative value can be used when specifying an offset relative to another date, e\.g\. \-2 days\.
       * - period
         - :ref:`PeriodEnum <type-daml-finance-interface-types-date-v3-rollconvention-periodenum-45289>`
         - A time period, e\.g\. a day, week, month or year of the stream\. If the periodMultiplier value is 0 (zero) then period must contain the value D (day)\.
       * - dayType
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `DayTypeEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daytypeenum-59419_>`_
         - In the case of an offset specified as a number of days, this element defines whether consideration is given as to whether a day is a good business day or not\. If a day type of business days is specified then non\-business days are ignored when calculating the offset\. The financial business centers to use for determination of business days are implied by the context in which this element is used\. This element must only be included when the offset is specified as a number of days\. If the offset is zero days then the dayType element should not be included\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `DateOffset <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-dateoffset-80142_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `DateOffset <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-dateoffset-80142_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daterelativetoenum-92127:

**data** `DateRelativeToEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daterelativetoenum-92127_>`_

  The specification of whether payments/resets occur relative to the first or last day of a
  calculation period\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodstartdate-10356:

  `CalculationPeriodStartDate <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodstartdate-10356_>`_

    Payments/Resets will occur relative to the first day of each calculation period\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodenddate-51553:

  `CalculationPeriodEndDate <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodenddate-51553_>`_

    Payments/Resets will occur relative to the last day of each calculation period\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `DateRelativeToEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daterelativetoenum-92127_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `DateRelativeToEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daterelativetoenum-92127_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daytypeenum-59419:

**data** `DayTypeEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daytypeenum-59419_>`_

  A day type classification used in counting the number of days between two dates\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-business-60860:

  `Business <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-business-60860_>`_

    When calculating the number of days between two dates the count includes only business
    days\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calendar-45432:

  `Calendar <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calendar-45432_>`_

    When calculating the number of days between two dates the count includes all calendar days\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-commoditybusiness-1692:

  `CommodityBusiness <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-commoditybusiness-1692_>`_

    When calculating the number of days between two dates the count includes only commodity
    business days\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-currencybusiness-13297:

  `CurrencyBusiness <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-currencybusiness-13297_>`_

    When calculating the number of days between two dates the count includes only currency
    business days\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-exchangebusiness-91323:

  `ExchangeBusiness <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-exchangebusiness-91323_>`_

    When calculating the number of days between two dates the count includes only stock
    exchange business days\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-scheduledtradingday-28649:

  `ScheduledTradingDay <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-scheduledtradingday-28649_>`_

    When calculating the number of days between two dates the count includes only scheduled
    trading days\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `DayTypeEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daytypeenum-59419_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `DayTypeEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daytypeenum-59419_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixedrateschedule-52475:

**data** `FixedRateSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixedrateschedule-52475_>`_

  Specify the fixed rate

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixedrateschedule-20378:

  `FixedRateSchedule <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixedrateschedule-20378_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - initialValue
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The initial rate or amount, as the case may be\. An initial rate of 5% would be represented as 0\.05\.
       * - step
         - \[`Step <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-step-53347_>`_\]
         - The schedule of step date and value pairs\. On each step date the associated step value becomes effective\. A list of steps may be ordered in the document by ascending step date\. An FpML document containing an unordered list of steps is still regarded as a conformant document\. type\_ \: Optional SpreadScheduleType

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `FixedRateSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixedrateschedule-52475_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `FixedRateSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixedrateschedule-52475_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixingdates-95248:

**data** `FixingDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixingdates-95248_>`_

  Specifies the fixing date relative to the reset date in terms of a business days offset and an
  associated set of financial business centers\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixingdates-3673:

  `FixingDates <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixingdates-3673_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - periodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         -
       * - period
         - :ref:`PeriodEnum <type-daml-finance-interface-types-date-v3-rollconvention-periodenum-45289>`
         -
       * - dayType
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `DayTypeEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daytypeenum-59419_>`_
         -
       * - businessDayConvention
         - :ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>`
         -
       * - businessCenters
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `FixingDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixingdates-95248_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `FixingDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixingdates-95248_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-floatingratecalculation-25241:

**data** `FloatingRateCalculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-floatingratecalculation-25241_>`_

  A type defining the floating rate and definitions
  relating to the calculation of floating rate amounts\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-floatingratecalculation-49348:

  `FloatingRateCalculation <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-floatingratecalculation-49348_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - floatingRateIndex
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - indexTenor
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Period <type-daml-finance-interface-types-date-v3-rollconvention-period-94990>`
         - The ISDA Designated Maturity, i\.e\., the tenor of the floating rate\. floatingRateMultiplierSchedule \: Optional Schedule \^ A rate multiplier or multiplier schedule to apply to the floating rate\. A multiplier schedule is expressed as explicit multipliers and dates\. In the case of a schedule, the step dates may be subject to adjustment in accordance with any adjustments specified in the calculationPeriodDatesAdjustments\. The multiplier can be a positive or negative decimal\. This element should only be included if the multiplier is not equal to 1 (one) for the term of the stream\.
       * - spreadSchedule
         - \[`SpreadSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-spreadschedule-59569_>`_\]
         - The ISDA Spread or a Spread schedule expressed as explicit spreads and dates\. In the case of a schedule, the step dates may be subject to adjustment in accordance with any adjustments specified in calculationPeriodDatesAdjustments\. The spread is a per annum rate, expressed as a decimal\. For purposes of determining a calculation period amount, if positive the spread will be added to the floating rate and if negative the spread will be subtracted from the floating rate\. A positive 10 basis point (0\.1%) spread would be represented as 0\.001\. rateTreatment \: Optional RateTreatmentEnum \^ The specification of any rate conversion which needs to be applied to the observed rate before being used in any calculations\. The two common conversions are for securities quoted on a bank discount basis which will need to be converted to either a Money Market Yield or Bond Equivalent Yield\. See the Annex to the 2000 ISDA Definitions, Section 7\.3\. Certain General Definitions Relating to Floating Rate Options, paragraphs (g) and (h) for definitions of these terms\. capRateSchedule \: \[StrikeSchedule\] \^ The cap rate or cap rate schedule, if any, which applies to the floating rate\. The cap rate (strike) is only required where the floating rate on a swap stream is capped at a certain level\. A cap rate schedule is expressed as explicit cap rates and dates and the step dates may be subject to adjustment in accordance with any adjustments specified in calculationPeriodDatesAdjustments\. The cap rate is assumed to be exclusive of any spread and is a per annum rate, expressed as a decimal\. A cap rate of 5% would be represented as 0\.05\. floorRateSchedule \: \[StrikeSchedule\] \^ The floor rate or floor rate schedule, if any, which applies to the floating rate\. The floor rate (strike) is only required where the floating rate on a swap stream is floored at a certain strike level\. A floor rate schedule is expressed as explicit floor rates and dates and the step dates may be subject to adjustment in accordance with any adjustments specified in calculationPeriodDatesAdjustments\. The floor rate is assumed to be exclusive of any spread and is a per annum rate, expressed as a decimal\. A floor rate of 5% would be represented as 0\.05\. initialRate \: Optional Decimal \^ The initial floating rate reset agreed between the principal parties involved in the trade\. This is assumed to be the first required reset rate for the first regular calculation period\. It should only be included when the rate is not equal to the rate published on the source implied by the floating rate index\. An initial rate of 5% would be represented as 0\.05\.
       * - finalRateRounding
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Rounding <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-rounding-1657_>`_
         - The rounding convention to apply to the final rate used in determination of a calculation period amount\. averagingMethod \: Optional AveragingMethodEnum \^ If averaging is applicable, this component specifies whether a weighted or unweighted average method of calculation is to be used\. The component must only be included when averaging applies\. negativeInterestRateTreatment \: Optional NegativeInterestRateTreatmentEnum \^ The specification of any provisions for calculating payment obligations when a floating rate is negative (either due to a quoted negative floating rate or by operation of a spread that is subtracted from the floating rate)\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `FloatingRateCalculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-floatingratecalculation-25241_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `FloatingRateCalculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-floatingratecalculation-25241_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxlinkednotionalschedule-96927:

**data** `FxLinkedNotionalSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxlinkednotionalschedule-96927_>`_

  The notional amount or notional amount schedule (FX linked)\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxlinkednotionalschedule-98300:

  `FxLinkedNotionalSchedule <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxlinkednotionalschedule-98300_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - constantNotionalScheduleReference
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - initialValue
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         -
       * - varyingNotionalCurrency
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - varyingNotionalFixingDates
         - `FixingDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixingdates-95248_>`_
         -
       * - fxSpotRateSource
         - `FxSpotRateSource <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxspotratesource-88026_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `FxLinkedNotionalSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxlinkednotionalschedule-96927_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `FxLinkedNotionalSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxlinkednotionalschedule-96927_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxspotratesource-88026:

**data** `FxSpotRateSource <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxspotratesource-88026_>`_

  A type defining the rate source and fixing time for
  an fx rate\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxspotratesource-26565:

  `FxSpotRateSource <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxspotratesource-26565_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - primaryRateSource
         - `InformationSource <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-informationsource-74973_>`_
         - The primary source for where the rate observation will occur\. Will typically be either a page or a reference bank published rate\. secondaryRateSource \: Optional InformationSource \^ An alternative, or secondary, source for where the rate observation will occur\. Will typically be either a page or a reference bank published rate\.
       * - fixingTime
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `BusinessCenterTime <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businesscentertime-84241_>`_
         - The time at which the spot currency exchange rate will be observed\. It is specified as a time in a business day calendar location, e\.g\., 11\:00am London time\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `FxSpotRateSource <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxspotratesource-88026_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `FxSpotRateSource <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxspotratesource-88026_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-hourminutetime-47030:

**type** `HourMinuteTime <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-hourminutetime-47030_>`_
  \= `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_

  A type defining a time specified in hh\:mm\:ss format where the second component must be '00',
  e\.g\., 11am would be represented as 11\:00\:00\.

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-informationsource-74973:

**data** `InformationSource <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-informationsource-74973_>`_

  A type defining the source for a piece of information (e\.g\. a rate refix or an fx fixing)\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-informationsource-58940:

  `InformationSource <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-informationsource-58940_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - rateSource
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - An information source for obtaining a market rate\. For example, Bloomberg, Reuters, Telerate etc\. rateSourcePage \: Optional RateSourcePage
       * - rateSourcePage
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A specific page for the rate source for obtaining a market rate\. rateSourcePageHeading \: Optional String \^ The heading for the rate source on a given rate source page\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `InformationSource <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-informationsource-74973_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `InformationSource <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-informationsource-74973_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedule-81262:

**data** `NotionalSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedule-81262_>`_

  The notional amount or notional amount schedule\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedule-74053:

  `NotionalSchedule <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedule-74053_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - notionalStepSchedule
         - `NotionalStepSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalstepschedule-46668_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `NotionalSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedule-81262_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `NotionalSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedule-81262_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedulevalue-85970:

**data** `NotionalScheduleValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedulevalue-85970_>`_

  Specifies how the notional schedule is defined\: either regular or fx linked\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalscheduleregular-7092:

  `NotionalSchedule_Regular <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalscheduleregular-7092_>`_ `NotionalSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedule-81262_>`_

    Regular notional schedule\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedulefxlinked-44562:

  `NotionalSchedule_FxLinked <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedulefxlinked-44562_>`_ `FxLinkedNotionalSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxlinkednotionalschedule-96927_>`_

    FX linked notional schedule\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `NotionalScheduleValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedulevalue-85970_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `NotionalScheduleValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalschedulevalue-85970_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalstepschedule-46668:

**data** `NotionalStepSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalstepschedule-46668_>`_

  The notional amount or notional amount schedule expressed as explicit outstanding notional
  amounts and dates\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalstepschedule-85867:

  `NotionalStepSchedule <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalstepschedule-85867_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - initialValue
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         -
       * - step
         - \[`Step <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-step-53347_>`_\]
         -
       * - currency
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `NotionalStepSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalstepschedule-46668_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `NotionalStepSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-notionalstepschedule-46668_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentdates-94528:

**data** `PaymentDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentdates-94528_>`_

  The payment dates schedule\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentdates-90807:

  `PaymentDates <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentdates-90807_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - calculationPeriodDatesReference
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - paymentFrequency
         - `PaymentFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentfrequency-30561_>`_
         -
       * - firstPaymentDate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         -
       * - lastRegularPaymentDate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         -
       * - payRelativeTo
         - `DateRelativeToEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daterelativetoenum-92127_>`_
         -
       * - paymentDaysOffset
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `DateOffset <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-dateoffset-80142_>`_
         -
       * - paymentDatesAdjustments
         - `BusinessDayAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-9111_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `PaymentDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentdates-94528_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `PaymentDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentdates-94528_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentfrequency-30561:

**data** `PaymentFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentfrequency-30561_>`_

  The frequency at which regular payment dates occur\. If the payment frequency is equal to the
  frequency defined in the calculation period dates component then one calculation period
  contributes to each payment amount\. If the payment frequency is less frequent than the
  frequency defined in the calculation period dates component then more than one calculation
  period will contribute to the payment amount\. A payment frequency more frequent than the
  calculation period frequency or one that is not a multiple of the calculation period frequency
  is invalid\. If the payment frequency is of value T (term), the period is defined by the
  swap\\swapStream\\calculationPerioDates\\effectiveDate and the
  swap\\swapStream\\calculationPerioDates\\terminationDate\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentfrequency-40030:

  `PaymentFrequency <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentfrequency-40030_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - periodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         -
       * - period
         - `PeriodExtendedEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-periodextendedenum-38896_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `PaymentFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentfrequency-30561_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `PaymentFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentfrequency-30561_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-periodextendedenum-38896:

**data** `PeriodExtendedEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-periodextendedenum-38896_>`_

  The period of a schedule, for example the calculation schedule\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-regular-10631:

  `Regular <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-regular-10631_>`_ :ref:`PeriodEnum <type-daml-finance-interface-types-date-v3-rollconvention-periodenum-45289>`


  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-t-90605:

  `T <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-t-90605_>`_


  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `PeriodExtendedEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-periodextendedenum-38896_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `PeriodExtendedEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-periodextendedenum-38896_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-principalexchanges-43211:

**data** `PrincipalExchanges <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-principalexchanges-43211_>`_

  A type defining which principal exchanges occur for
  the stream\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-principalexchanges-27040:

  `PrincipalExchanges <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-principalexchanges-27040_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - initialExchange
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - A true/false flag to indicate whether there is an initial exchange of principal on the effective date\.
       * - finalExchange
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - A true/false flag to indicate whether there is a final exchange of principal on the termination date\.
       * - intermediateExchange
         - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
         - A true/false flag to indicate whether there are intermediate or interim exchanges of principal during the term of the swap\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `PrincipalExchanges <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-principalexchanges-43211_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `PrincipalExchanges <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-principalexchanges-43211_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-ratetypevalue-70501:

**data** `RateTypeValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-ratetypevalue-70501_>`_

  Specifies whether the swapStream has a fixed or a floating rate\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-ratetypefixed-58433:

  `RateType_Fixed <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-ratetypefixed-58433_>`_ `FixedRateSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixedrateschedule-52475_>`_

    Fixed rate\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-ratetypefloating-46162:

  `RateType_Floating <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-ratetypefloating-46162_>`_ `FloatingRateCalculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-floatingratecalculation-25241_>`_

    Floating rate\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `RateTypeValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-ratetypevalue-70501_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `RateTypeValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-ratetypevalue-70501_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdates-28315:

**data** `ResetDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdates-28315_>`_

  The reset dates schedule\. This only applies for a floating rate stream\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdates-27408:

  `ResetDates <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdates-27408_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - calculationPeriodDatesReference
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - resetRelativeTo
         - `DateRelativeToEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-daterelativetoenum-92127_>`_
         -
       * - fixingDates
         - `FixingDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixingdates-95248_>`_
         -
       * - resetFrequency
         - `ResetFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetfrequency-57746_>`_
         -
       * - resetDatesAdjustments
         - `ResetDatesAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdatesadjustments-44820_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ResetDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdates-28315_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ResetDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdates-28315_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdatesadjustments-44820:

**data** `ResetDatesAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdatesadjustments-44820_>`_

  The business day convention to apply to each reset date if it would otherwise fall on a day
  that is not a business day in the specified financial business centers\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdatesadjustments-41001:

  `ResetDatesAdjustments <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdatesadjustments-41001_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - businessDayConvention
         - :ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>`
         -
       * - businessCenters
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ResetDatesAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdatesadjustments-44820_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ResetDatesAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdatesadjustments-44820_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetfrequency-57746:

**data** `ResetFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetfrequency-57746_>`_

  The frequency at which reset dates occur\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetfrequency-34569:

  `ResetFrequency <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetfrequency-34569_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - periodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         -
       * - period
         - `PeriodExtendedEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-periodextendedenum-38896_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ResetFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetfrequency-57746_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ResetFrequency <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetfrequency-57746_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-rounding-1657:

**data** `Rounding <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-rounding-1657_>`_

  A type defining a rounding direction and precision to be used in the rounding of a rate\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-rounding-71494:

  `Rounding <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-rounding-71494_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - roundingDirection
         - `RoundingDirectionEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-roundingdirectionenum-88988_>`_
         - Specifies the rounding direction\.
       * - precision
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         - Specifies the rounding precision in terms of a number of decimal places\. Note how a percentage rate rounding of 5 decimal places is expressed as a rounding precision of 7 in the FpML document since the percentage is expressed as a decimal, e\.g\. 9\.876543% (or 0\.09876543) being rounded to the nearest 5 decimal places is 9\.87654% (or 0\.0987654)\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Rounding <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-rounding-1657_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Rounding <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-rounding-1657_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-roundingdirectionenum-88988:

**data** `RoundingDirectionEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-roundingdirectionenum-88988_>`_

  The method of rounding a fractional number\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-up-43299:

  `Up <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-up-43299_>`_

    A fractional number will be rounded up to the specified number of decimal places (the
    precision)\. For example, 5\.21 and 5\.25 rounded up to 1 decimal place are 5\.3 and 5\.3
    respectively\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-down-29126:

  `Down <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-down-29126_>`_

    A fractional number will be rounded down to the specified number of decimal places (the
    precision)\. For example, 5\.29 and 5\.25 rounded down to 1 decimal place are 5\.2 and 5\.2
    respectively\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-nearest-14973:

  `Nearest <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-nearest-14973_>`_

    A fractional number will be rounded either up or down to the specified number of decimal
    places (the precision) depending on its value\. For example, 5\.24 would be rounded down to 5\.2
    and 5\.25 would be rounded up to 5\.3 if a precision of 1 decimal place were specified\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `RoundingDirectionEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-roundingdirectionenum-88988_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `RoundingDirectionEnum <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-roundingdirectionenum-88988_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-spreadschedule-59569:

**data** `SpreadSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-spreadschedule-59569_>`_

  Adds an optional spread type element to the Schedule to identify a long or short spread value\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-spreadschedule-88454:

  `SpreadSchedule <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-spreadschedule-88454_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - initialValue
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The initial rate or amount, as the case may be\. An initial rate of 5% would be represented as 0\.05\. step \: \[Step\] \^ The schedule of step date and value pairs\. On each step date the associated step value becomes effective\. A list of steps may be ordered in the document by ascending step date\. An FpML document containing an unordered list of steps is still regarded as a conformant document\. type\_ \: Optional SpreadScheduleType

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `SpreadSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-spreadschedule-59569_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `SpreadSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-spreadschedule-59569_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-step-53347:

**data** `Step <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-step-53347_>`_

  The schedule of step date and non\-negative value pairs\. On each step date the associated step
  value becomes effective\. A list of steps may be ordered in the document by ascending step date\.
  An FpML document containing an unordered list of steps is still regarded as a conformant
  document\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-step-25240:

  `Step <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-step-25240_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - stepDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         -
       * - stepValue
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Step <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-step-53347_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Step <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-step-53347_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubcalculationperiodamount-55158:

**data** `StubCalculationPeriodAmount <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubcalculationperiodamount-55158_>`_

  The stub calculation period amount parameters\. This element must only be included if there is
  an initial or final stub calculation period\. Even then, it must only be included if either the
  stub references a different floating rate tenor to the regular calculation periods, or if the
  stub is calculated as a linear interpolation of two different floating rate tenors, or if a
  specific stub rate or stub amount has been negotiated\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubcalculationperiodamount-88363:

  `StubCalculationPeriodAmount <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubcalculationperiodamount-88363_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - calculationPeriodDatesReference
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - initialStub
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `StubValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvalue-4311_>`_
         -
       * - finalStub
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `StubValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvalue-4311_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `StubCalculationPeriodAmount <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubcalculationperiodamount-55158_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `StubCalculationPeriodAmount <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubcalculationperiodamount-55158_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-62557:

**data** `StubFloatingRate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-62557_>`_

  The rates to be applied to the initial or final stub may be the linear interpolation of two
  different rates\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-7678:

  `StubFloatingRate <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-7678_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - floatingRateIndex
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - indexTenor
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`Period <type-daml-finance-interface-types-date-v3-rollconvention-period-94990>`
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `StubFloatingRate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-62557_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `StubFloatingRate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-62557_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvalue-4311:

**data** `StubValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvalue-4311_>`_

  Specifies how the stub amount is calculated\. A single floating rate tenor different to that
  used for the regular part of the calculation periods schedule may be specified, or two floating
  tenors may be specified\. If two floating rate tenors are specified then Linear Interpolation
  (in accordance with the 2000 ISDA Definitions, Section 8\.3\. Interpolation) is assumed to apply\.
  Alternatively, an actual known stub rate or stub amount may be specified\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvaluefloatingrate-91634:

  `StubValue_FloatingRate <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvaluefloatingrate-91634_>`_ \[`StubFloatingRate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-62557_>`_\]

    The rates to be applied to the initial or final stub may be the linear interpolation of two
    different rates\. While the majority of the time, the rate indices will be the same as that
    specified in the stream and only the tenor itself will be different, it is possible to
    specift two different rates\. For example, a 2 month stub period may use the linear
    interpolation of a 1 month and 3 month rate\. The different rates would be specified in this
    component\. Note that a maximum of two rates can be specified\. If a stub period uses the
    same floating rate index, including tenor, as the regular calculation periods then this
    should not be specified again within this component, i\.e\., the stub calculation period
    amount component may not need to be specified even if there is an initial or final stub
    period\. If a stub period uses a different floating rate index compared to the regular
    calculation periods then this should be specified within this component\. If specified here,
    they are likely to have id attributes, allowing them to be referenced from within the
    cashflows component\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvaluestubrate-6456:

  `StubValue_StubRate <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvaluestubrate-6456_>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

    An actual rate to apply for the initial or final stub period may have been agreed between
    the principal parties (in a similar way to how an initial rate may have been agreed for the
    first regular period)\. If an actual stub rate has been agreed then it would be included in
    this component\. It will be a per annum rate, expressed as a decimal\. A stub rate of 5%
    would be represented as 0\.05\.
    | StubValue\_StubAmount Money
    \^ An actual amount to apply for the initial or final stub period may have been agreed between
    the two parties\. If an actual stub amount has been agreed then it would be included in this
    component\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `StubValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvalue-4311_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `StubValue <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubvalue-4311_>`_

.. _type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822:

**data** `SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822_>`_

  The swap streams, describing each leg of the swap\.

  .. _constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-53745:

  `SwapStream <constr-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-53745_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - payerPartyReference
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - receiverPartyReference
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         -
       * - calculationPeriodDates
         - `CalculationPeriodDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddates-23760_>`_
         -
       * - paymentDates
         - `PaymentDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentdates-94528_>`_
         -
       * - resetDates
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `ResetDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdates-28315_>`_
         -
       * - calculationPeriodAmount
         - `CalculationPeriodAmount <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperiodamount-86226_>`_
         -
       * - stubCalculationPeriodAmount
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `StubCalculationPeriodAmount <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubcalculationperiodamount-55158_>`_
         -
       * - principalExchanges
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `PrincipalExchanges <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-principalexchanges-43211_>`_
         -

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822_>`_
