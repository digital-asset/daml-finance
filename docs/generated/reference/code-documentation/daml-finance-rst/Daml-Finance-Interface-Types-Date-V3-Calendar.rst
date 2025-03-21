.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-types-date-v3-calendar-20021:

Daml.Finance.Interface.Types.Date.V3.Calendar
=============================================

Data Types
----------

.. _type-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-71551:

**data** `BusinessDayAdjustment <type-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-71551_>`_

  A data type to define how non\-business days are adjusted\.

  .. _constr-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-93800:

  `BusinessDayAdjustment <constr-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-93800_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - calendarIds
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         - A list of calendar ids to define holidays\.
       * - convention
         - `BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112_>`_
         - The business day convention used for the adjustment\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BusinessDayAdjustment <type-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-71551_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BusinessDayAdjustment <type-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-71551_>`_

.. _type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112:

**data** `BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112_>`_

  An enum type to specify how a non\-business day is adjusted\.

  .. _constr-daml-finance-interface-types-date-v3-calendar-following-20900:

  `Following <constr-daml-finance-interface-types-date-v3-calendar-following-20900_>`_

    Adjust a non\-business day to the next business day\.

  .. _constr-daml-finance-interface-types-date-v3-calendar-modifiedfollowing-531:

  `ModifiedFollowing <constr-daml-finance-interface-types-date-v3-calendar-modifiedfollowing-531_>`_

    Adjust a non\-business day to the next business day
    unless it is not in the same month\. In this case use
    the previous business day\.

  .. _constr-daml-finance-interface-types-date-v3-calendar-modifiedpreceding-98945:

  `ModifiedPreceding <constr-daml-finance-interface-types-date-v3-calendar-modifiedpreceding-98945_>`_

    Adjust a non\-business day to the previous business day
    unless it is not in the same month\. In this case use
    the next business day\.

  .. _constr-daml-finance-interface-types-date-v3-calendar-noadjustment-12134:

  `NoAdjustment <constr-daml-finance-interface-types-date-v3-calendar-noadjustment-12134_>`_

    Non\-business days are not adjusted\.

  .. _constr-daml-finance-interface-types-date-v3-calendar-preceding-85670:

  `Preceding <constr-daml-finance-interface-types-date-v3-calendar-preceding-85670_>`_

    Adjust a non\-business day to the previous business day\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112_>`_

.. _type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370:

**data** `HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370_>`_

  Holiday Calendar Data used to define holidays (non\-business days)\.

  .. _constr-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-20901:

  `HolidayCalendarData <constr-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-20901_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The id of the holiday calendar\.
       * - weekend
         - \[`DayOfWeek <https://docs.daml.com/daml/stdlib/DA-Date.html#type-da-date-types-dayofweek-18120>`_\]
         - A list of week days defining the weekend\.
       * - holidays
         - \[`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_\]
         - A list of dates defining holidays\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370_>`_

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ :ref:`HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871>` :ref:`HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417>` :ref:`GetCalendar <type-daml-finance-data-v4-reference-holidaycalendar-getcalendar-25827>` `HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370_>`_
