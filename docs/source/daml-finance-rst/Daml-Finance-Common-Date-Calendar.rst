.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-common-date-calendar-52063:

Module Daml.Finance.Common.Date.Calendar
========================================

Data Types
----------

.. _type-daml-finance-common-date-calendar-businessdayadjustment-34753:

**data** `BusinessDayAdjustment <type-daml-finance-common-date-calendar-businessdayadjustment-34753_>`_

  A data type to define how non\-business days are adjusted\.
  
  .. _constr-daml-finance-common-date-calendar-businessdayadjustment-73530:
  
  `BusinessDayAdjustment <constr-daml-finance-common-date-calendar-businessdayadjustment-73530_>`_
  
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
         - `BusinessDayConventionEnum <type-daml-finance-common-date-calendar-businessdayconventionenum-67582_>`_
         - The business day convention used for the adjustment\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BusinessDayAdjustment <type-daml-finance-common-date-calendar-businessdayadjustment-34753_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BusinessDayAdjustment <type-daml-finance-common-date-calendar-businessdayadjustment-34753_>`_

.. _type-daml-finance-common-date-calendar-businessdayconventionenum-67582:

**data** `BusinessDayConventionEnum <type-daml-finance-common-date-calendar-businessdayconventionenum-67582_>`_

  An enum type to specify how a non\-business day is adjusted\.
  
  .. _constr-daml-finance-common-date-calendar-following-49838:
  
  `FOLLOWING <constr-daml-finance-common-date-calendar-following-49838_>`_
  
    Adjust a non\-business day to the next business day\.
  
  .. _constr-daml-finance-common-date-calendar-modfollowing-42681:
  
  `MODFOLLOWING <constr-daml-finance-common-date-calendar-modfollowing-42681_>`_
  
    Adjust a non\-business day to the next business day
    unless it is not in the same month\. In this case use
    the previous business day\.
  
  .. _constr-daml-finance-common-date-calendar-modpreceding-4611:
  
  `MODPRECEDING <constr-daml-finance-common-date-calendar-modpreceding-4611_>`_
  
    Adjust a non\-business day to the previous business day
    unless it is not in the same month\. In this case use
    the next business day\.
  
  .. _constr-daml-finance-common-date-calendar-none-44306:
  
  `NONE <constr-daml-finance-common-date-calendar-none-44306_>`_
  
    Non\-business days are not adjusted\.
  
  .. _constr-daml-finance-common-date-calendar-preceding-50800:
  
  `PRECEDING <constr-daml-finance-common-date-calendar-preceding-50800_>`_
  
    Adjust a non\-business day to the previous business day\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `BusinessDayConventionEnum <type-daml-finance-common-date-calendar-businessdayconventionenum-67582_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `BusinessDayConventionEnum <type-daml-finance-common-date-calendar-businessdayconventionenum-67582_>`_

.. _type-daml-finance-common-date-calendar-holidaycalendardata-72016:

**data** `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_

  Holiday Calendar Data used to define holidays\.
  
  .. _constr-daml-finance-common-date-calendar-holidaycalendardata-81963:
  
  `HolidayCalendarData <constr-daml-finance-common-date-calendar-holidaycalendardata-81963_>`_
  
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
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_
  
  **instance** `HasExercise <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercise-70422>`_ :ref:`HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891>` GetCalendar `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ :ref:`HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891>` :ref:`HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389>` GetCalendar `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_
  
  **instance** `HasFromAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanychoice-81184>`_ :ref:`HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891>` GetCalendar `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_
  
  **instance** `HasToAnyChoice <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanychoice-82571>`_ :ref:`HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891>` GetCalendar `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_

Functions
---------

.. _function-daml-finance-common-date-calendar-merge-1552:

`merge <function-daml-finance-common-date-calendar-merge-1552_>`_
  \: \[`HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_\] \-\> `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_
  
  Merge multiple holiday calendars into a single one\. ``id``\\s are concatenated by ``,``\.

.. _function-daml-finance-common-date-calendar-isholiday-1952:

`isHoliday <function-daml-finance-common-date-calendar-isholiday-1952_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
  
  Check if Date is a holiday

.. _function-daml-finance-common-date-calendar-isbusinessday-2112:

`isBusinessDay <function-daml-finance-common-date-calendar-isbusinessday-2112_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
  
  Check if Date is a business day

.. _function-daml-finance-common-date-calendar-nextbusinessday-71643:

`nextBusinessDay <function-daml-finance-common-date-calendar-nextbusinessday-71643_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Get next business day

.. _function-daml-finance-common-date-calendar-previousbusinessday-35555:

`previousBusinessDay <function-daml-finance-common-date-calendar-previousbusinessday-35555_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Get previous business day

.. _function-daml-finance-common-date-calendar-nextorsamebusinessday-8046:

`nextOrSameBusinessDay <function-daml-finance-common-date-calendar-nextorsamebusinessday-8046_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Get next or same business day

.. _function-daml-finance-common-date-calendar-previousorsamebusinessday-91742:

`previousOrSameBusinessDay <function-daml-finance-common-date-calendar-previousorsamebusinessday-91742_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Get previous or same business day

.. _function-daml-finance-common-date-calendar-nextsameorlastinmonthbusinessday-23572:

`nextSameOrLastInMonthBusinessDay <function-daml-finance-common-date-calendar-nextsameorlastinmonthbusinessday-23572_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Get next or same business day if before end of month\. Otherwise get last business day in month\.

.. _function-daml-finance-common-date-calendar-previoussameorfirstinmonthbusinessday-20927:

`previousSameOrFirstInMonthBusinessDay <function-daml-finance-common-date-calendar-previoussameorfirstinmonthbusinessday-20927_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Get previous or same business day if before end of month\. Otherwise get first business day in month\.

.. _function-daml-finance-common-date-calendar-addbusinessdays-94508:

`addBusinessDays <function-daml-finance-common-date-calendar-addbusinessdays-94508_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Add business days to a Date

.. _function-daml-finance-common-date-calendar-adjustdate-89610:

`adjustDate <function-daml-finance-common-date-calendar-adjustdate-89610_>`_
  \: `HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016_>`_ \-\> `BusinessDayConventionEnum <type-daml-finance-common-date-calendar-businessdayconventionenum-67582_>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
  
  Adjust date according to the given business day convention
