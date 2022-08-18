.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-common-date-schedule-66964:

Module Daml.Finance.Common.Date.Schedule
========================================

Data Types
----------

.. _type-daml-finance-common-date-schedule-frequency-55811:

**data** `Frequency <type-daml-finance-common-date-schedule-frequency-55811_>`_

  Frequency of a periodic schedule\.
  
  .. _constr-daml-finance-common-date-schedule-frequency-65932:
  
  `Frequency <constr-daml-finance-common-date-schedule-frequency-65932_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - period
         - :ref:`PeriodEnum <type-daml-finance-common-date-rollconvention-periodenum-40915>`
         - The period, e\.g\. day, month, etc\.
       * - periodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         - The period multiplier\.
       * - rollConvention
         - :ref:`RollConventionEnum <type-daml-finance-common-date-rollconvention-rollconventionenum-16664>`
         - The roll convention\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Frequency <type-daml-finance-common-date-schedule-frequency-55811_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Frequency <type-daml-finance-common-date-schedule-frequency-55811_>`_

.. _type-daml-finance-common-date-schedule-periodicschedule-45070:

**data** `PeriodicSchedule <type-daml-finance-common-date-schedule-periodicschedule-45070_>`_

  A periodic schedule\.
  
  .. _constr-daml-finance-common-date-schedule-periodicschedule-99911:
  
  `PeriodicSchedule <constr-daml-finance-common-date-schedule-periodicschedule-99911_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - effectiveDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Effective date, i\.e\. the (unadjusted) start date of the first period\.
       * - terminationDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Termination date, i\.e\. the (unadjusted) end date of the last period\.
       * - firstRegularPeriodStartDate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The (unadjusted) start date of the first regular period (optional)\.
       * - lastRegularPeriodEndDate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The (unadjusted) end date of the last regular period (optional)\.
       * - frequency
         - `Frequency <type-daml-finance-common-date-schedule-frequency-55811_>`_
         - The frequency of the periodic schedule\.
       * - businessDayAdjustment
         - :ref:`BusinessDayAdjustment <type-daml-finance-common-date-calendar-businessdayadjustment-34753>`
         - The business day adjustment to determine adjusted dates\.
       * - effectiveDateBusinessDayAdjustment
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`BusinessDayAdjustment <type-daml-finance-common-date-calendar-businessdayadjustment-34753>`
         - The (optional) business day adjustment of the effective date
       * - terminationDateBusinessDayAdjustment
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`BusinessDayAdjustment <type-daml-finance-common-date-calendar-businessdayadjustment-34753>`
         - The (optional) business day adjustment of the termination date
       * - stubPeriodType
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `StubPeriodTypeEnum <type-daml-finance-common-date-schedule-stubperiodtypeenum-30472_>`_
         - An optional stub to define a stub implicitly and not via ``firstRegularPeriodStartDate`` or ``lastRegularPeriodEndDate``\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `PeriodicSchedule <type-daml-finance-common-date-schedule-periodicschedule-45070_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `PeriodicSchedule <type-daml-finance-common-date-schedule-periodicschedule-45070_>`_

.. _type-daml-finance-common-date-schedule-schedule-53649:

**type** `Schedule <type-daml-finance-common-date-schedule-schedule-53649_>`_
  \= \[`SchedulePeriod <type-daml-finance-common-date-schedule-scheduleperiod-62172_>`_\]
  
  A schedule defined by a list of periods\.

.. _type-daml-finance-common-date-schedule-scheduleperiod-62172:

**data** `SchedulePeriod <type-daml-finance-common-date-schedule-scheduleperiod-62172_>`_

  A single period in a schedule\.
  
  .. _constr-daml-finance-common-date-schedule-scheduleperiod-37325:
  
  `SchedulePeriod <constr-daml-finance-common-date-schedule-scheduleperiod-37325_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - adjustedEndDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Adjusted end date\.
       * - adjustedStartDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Adjusted start date\.
       * - unadjustedEndDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Unadjusted end date\.
       * - unadjustedStartDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Unadjusted start date\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `SchedulePeriod <type-daml-finance-common-date-schedule-scheduleperiod-62172_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `SchedulePeriod <type-daml-finance-common-date-schedule-scheduleperiod-62172_>`_

.. _type-daml-finance-common-date-schedule-stubperiodtypeenum-30472:

**data** `StubPeriodTypeEnum <type-daml-finance-common-date-schedule-stubperiodtypeenum-30472_>`_

  An enum type to specify a stub\.
  
  .. _constr-daml-finance-common-date-schedule-longfinal-18838:
  
  `LONG_FINAL <constr-daml-finance-common-date-schedule-longfinal-18838_>`_
  
    A long (more than one period) final stub\.
  
  .. _constr-daml-finance-common-date-schedule-longinitial-37240:
  
  `LONG_INITIAL <constr-daml-finance-common-date-schedule-longinitial-37240_>`_
  
    A long (more than one period) initial stub\.
  
  .. _constr-daml-finance-common-date-schedule-shortfinal-34671:
  
  `SHORT_FINAL <constr-daml-finance-common-date-schedule-shortfinal-34671_>`_
  
    A short (less than one period) final stub\.
  
  .. _constr-daml-finance-common-date-schedule-shortinitial-14265:
  
  `SHORT_INITIAL <constr-daml-finance-common-date-schedule-shortinitial-14265_>`_
  
    A short (less than one period) initial stub\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `StubPeriodTypeEnum <type-daml-finance-common-date-schedule-stubperiodtypeenum-30472_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `StubPeriodTypeEnum <type-daml-finance-common-date-schedule-stubperiodtypeenum-30472_>`_

Functions
---------

.. _function-daml-finance-common-date-schedule-createschedule-17479:

`createSchedule <function-daml-finance-common-date-schedule-createschedule-17479_>`_
  \: \[:ref:`HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016>`\] \-\> `PeriodicSchedule <type-daml-finance-common-date-schedule-periodicschedule-45070_>`_ \-\> `Schedule <type-daml-finance-common-date-schedule-schedule-53649_>`_
  
  Generate schedule from a periodic schedule\.
