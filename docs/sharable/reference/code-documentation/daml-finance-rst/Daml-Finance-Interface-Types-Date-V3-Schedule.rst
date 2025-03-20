.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-types-date-v3-schedule-94670:

Daml.Finance.Interface.Types.Date.V3.Schedule
=============================================

Data Types
----------

.. _type-daml-finance-interface-types-date-v3-schedule-frequency-37405:

**data** `Frequency <type-daml-finance-interface-types-date-v3-schedule-frequency-37405_>`_

  Frequency of a periodic schedule\.

  .. _constr-daml-finance-interface-types-date-v3-schedule-frequency-33990:

  `Frequency <constr-daml-finance-interface-types-date-v3-schedule-frequency-33990_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - period
         - :ref:`Period <type-daml-finance-interface-types-date-v3-rollconvention-period-94990>`
         - The period (e\.g\., 1D, 3M, 1Y)\.
       * - rollConvention
         - :ref:`RollConventionEnum <type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490>`
         - The roll convention\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `Frequency <type-daml-finance-interface-types-date-v3-schedule-frequency-37405_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `Frequency <type-daml-finance-interface-types-date-v3-schedule-frequency-37405_>`_

.. _type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368:

**data** `PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368_>`_

  A periodic schedule\.

  .. _constr-daml-finance-interface-types-date-v3-schedule-periodicschedule-24577:

  `PeriodicSchedule <constr-daml-finance-interface-types-date-v3-schedule-periodicschedule-24577_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - effectiveDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Effective date, i\.e\., the (unadjusted) start date of the first period\.
       * - terminationDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - Termination date, i\.e\., the (unadjusted) end date of the last period\.
       * - firstRegularPeriodStartDate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The (unadjusted) start date of the first regular period (optional)\.
       * - lastRegularPeriodEndDate
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The (unadjusted) end date of the last regular period (optional)\.
       * - frequency
         - `ScheduleFrequency <type-daml-finance-interface-types-date-v3-schedule-schedulefrequency-11056_>`_
         - The frequency of the periodic schedule\.
       * - businessDayAdjustment
         - :ref:`BusinessDayAdjustment <type-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-71551>`
         - The business day adjustment to determine adjusted dates\.
       * - effectiveDateBusinessDayAdjustment
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`BusinessDayAdjustment <type-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-71551>`
         - The (optional) business day adjustment of the effective date
       * - terminationDateBusinessDayAdjustment
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`BusinessDayAdjustment <type-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-71551>`
         - The (optional) business day adjustment of the termination date
       * - stubPeriodType
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `StubPeriodTypeEnum <type-daml-finance-interface-types-date-v3-schedule-stubperiodtypeenum-99734_>`_
         - An optional stub to define a stub implicitly and not via ``firstRegularPeriodStartDate`` or ``lastRegularPeriodEndDate``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368_>`_

.. _type-daml-finance-interface-types-date-v3-schedule-schedule-18327:

**type** `Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327_>`_
  \= \[`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606_>`_\]

  A schedule defined by a list of periods\.

.. _type-daml-finance-interface-types-date-v3-schedule-schedulefrequency-11056:

**data** `ScheduleFrequency <type-daml-finance-interface-types-date-v3-schedule-schedulefrequency-11056_>`_

  Frequency of a schedule\. It can be specified as a regular frequency or
  as ``SinglePeriod``\.

  .. _constr-daml-finance-interface-types-date-v3-schedule-periodic-85058:

  `Periodic <constr-daml-finance-interface-types-date-v3-schedule-periodic-85058_>`_ `Frequency <type-daml-finance-interface-types-date-v3-schedule-frequency-37405_>`_

    Periodic frequency (e\.g\. 1D, 3M, 1Y)\.

  .. _constr-daml-finance-interface-types-date-v3-schedule-singleperiod-55098:

  `SinglePeriod <constr-daml-finance-interface-types-date-v3-schedule-singleperiod-55098_>`_

    Used for schedules that have exactly one regular period covering their full term
    (from ``effectiveDate`` to ``terminationDate``)\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `ScheduleFrequency <type-daml-finance-interface-types-date-v3-schedule-schedulefrequency-11056_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `ScheduleFrequency <type-daml-finance-interface-types-date-v3-schedule-schedulefrequency-11056_>`_

.. _type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606:

**data** `SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606_>`_

  A single period in a schedule\.

  .. _constr-daml-finance-interface-types-date-v3-schedule-scheduleperiod-78303:

  `SchedulePeriod <constr-daml-finance-interface-types-date-v3-schedule-scheduleperiod-78303_>`_

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
       * - stubType
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `StubPeriodTypeEnum <type-daml-finance-interface-types-date-v3-schedule-stubperiodtypeenum-99734_>`_
         - Indicates whether this period is a stub (and if so, what type of stub it is)

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606_>`_

.. _type-daml-finance-interface-types-date-v3-schedule-stubperiodtypeenum-99734:

**data** `StubPeriodTypeEnum <type-daml-finance-interface-types-date-v3-schedule-stubperiodtypeenum-99734_>`_

  An enum type to specify a stub\.

  .. _constr-daml-finance-interface-types-date-v3-schedule-longfinal-79474:

  `LongFinal <constr-daml-finance-interface-types-date-v3-schedule-longfinal-79474_>`_

    A long (more than one period) final stub\.

  .. _constr-daml-finance-interface-types-date-v3-schedule-longinitial-63504:

  `LongInitial <constr-daml-finance-interface-types-date-v3-schedule-longinitial-63504_>`_

    A long (more than one period) initial stub\.

  .. _constr-daml-finance-interface-types-date-v3-schedule-shortfinal-87465:

  `ShortFinal <constr-daml-finance-interface-types-date-v3-schedule-shortfinal-87465_>`_

    A short (less than one period) final stub\.

  .. _constr-daml-finance-interface-types-date-v3-schedule-shortinitial-94551:

  `ShortInitial <constr-daml-finance-interface-types-date-v3-schedule-shortinitial-94551_>`_

    A short (less than one period) initial stub\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `StubPeriodTypeEnum <type-daml-finance-interface-types-date-v3-schedule-stubperiodtypeenum-99734_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `StubPeriodTypeEnum <type-daml-finance-interface-types-date-v3-schedule-stubperiodtypeenum-99734_>`_
