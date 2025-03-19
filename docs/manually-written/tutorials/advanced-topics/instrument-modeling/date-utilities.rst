.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Date Utility Functions: Calendar, Schedule, and Day Count
#########################################################

The Daml Finance library contains date-related utility functions for
implementing industry-standard conventions. These functions are used
internally in instruments like bonds and swaps, but they can also be helpful in
your own custom use cases. For example, you can use these functions to:

- understand and validate the implementation of existing Daml Finance
  instruments
- compose existing Daml Finance instruments; for example, define a series of
  zero-coupon bonds according to a specific schedule of maturity dates
- develop your own instruments that depend on business day shifts, schedules,
  or day count conventions
- work with non-financial use cases

Calendar
========

The :ref:`Calendar <module-daml-finance-util-v4-date-calendar-71711>` module contains various utility
functions related to business days and date adjustments.

To distinguish business days and non-business days, set a holiday calendar. The :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`
type defines the set of non-business days by specifying the days of the week that belong to the
weekend, as well as the dates of designated holidays. The following example sets Saturday and Sunday
as weekend days and specifies some additional holidays:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Calendar.daml
  :language: daml
  :start-after: -- CREATE_HOLIDAY_CALENDAR_DATA_BEGIN
  :end-before: -- CREATE_HOLIDAY_CALENDAR_DATA_END

To check whether a given date is a business day, use the ``isBusinessDay`` function:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Calendar.daml
  :language: daml
  :start-after: -- TEST_IS_BUSINESS_DAY_BEGIN
  :end-before: -- TEST_IS_BUSINESS_DAY_END

You can also get the previous and next business day:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Calendar.daml
  :language: daml
  :start-after: -- TEST_PREVIOUS_BUSINESS_DAY_BEGIN
  :end-before: -- TEST_PREVIOUS_BUSINESS_DAY_END

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Calendar.daml
  :language: daml
  :start-after: -- TEST_NEXT_BUSINESS_DAY_BEGIN
  :end-before: -- TEST_NEXT_BUSINESS_DAY_END

To find the date that corresponds to a specific number of business
days in the future, use ``addBusinessDays``:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Calendar.daml
  :language: daml
  :start-after: -- TEST_ADD_BUSINESS_DAYS_BEGIN
  :end-before: -- TEST_ADD_BUSINESS_DAYS_END

You can also get the date that corresponds to a number of business days in the *past*, by passing in
a negative offset:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Calendar.daml
  :language: daml
  :start-after: -- TEST_ADD_BUSINESS_DAYS_NEGATIVE_OFFSET_BEGIN
  :end-before: -- TEST_ADD_BUSINESS_DAYS_NEGATIVE_OFFSET_END

You can adjust a date according to a business day convention:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Calendar.daml
  :language: daml
  :start-after: -- TEST_ADJUST_DATE_BEGIN
  :end-before: -- TEST_ADJUST_DATE_END

For the full list of available conventions, see the
:ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>`
reference. Use them to specify how non-business days are adjusted, including when the next or
previous business day is around the end of the month.

RollConvention
==============

The :ref:`RollConvention <module-daml-finance-util-v4-date-rollconvention-61455>` module provides
utility functions to add a period to a given date.

For example, you can add a day, week, month, or year to a date:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/RollConvention.daml
  :language: daml
  :start-after: -- TEST_ADD_PERIOD_DAY_WEEK_MONTH_YEAR_BEGIN
  :end-before: -- TEST_ADD_PERIOD_DAY_WEEK_MONTH_YEAR_END

This function can also handle edge cases. For example, when you add a period of one month to a date,
the function makes adjustments if the resulting month has fewer days than the starting month:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/RollConvention.daml
  :language: daml
  :start-after: -- TEST_ADD_PERIOD_MONTH_END_EDGE_CASE_BEGIN
  :end-before: -- TEST_ADD_PERIOD_MONTH_END_EDGE_CASE_END

To *subtract* a period of time, use a negative offset:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/RollConvention.daml
  :language: daml
  :start-after: -- TEST_ADD_PERIOD_NEGATIVE_OFFSET_BEGIN
  :end-before: -- TEST_ADD_PERIOD_NEGATIVE_OFFSET_END

You might need to find the start date of the next period according to a specific market
convention, as described in the
:ref:`RollConventionEnum <type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490>`
reference.
The *Roll convention* specifies when the date is rolled.

For example, you can define an end-of-month roll:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/RollConvention.daml
  :language: daml
  :start-after: -- TEST_NEXT_EOM_BEGIN
  :end-before: -- TEST_NEXT_EOM_END

You can also set the roll to a specific day of the month:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/RollConvention.daml
  :language: daml
  :start-after: -- TEST_NEXT_DOM1_BEGIN
  :end-before: -- TEST_NEXT_DOM1_END

If the resulting month does not have enough days, the last day of the month is used instead:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/RollConvention.daml
  :language: daml
  :start-after: -- TEST_NEXT_DOM_PAST_EOM_BEGIN
  :end-before: -- TEST_NEXT_DOM_PAST_EOM_END

This function also takes leap days into account:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/RollConvention.daml
  :language: daml
  :start-after: -- TEST_NEXT_DOM_PAST_EOFEB_BEGIN
  :end-before: -- TEST_NEXT_DOM_PAST_EOFEB_END

The following example shows you how to roll to the *previous* period:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/RollConvention.daml
  :language: daml
  :start-after: -- TEST_PREVIOUS_DOM_FROM_EOFEB_BEGIN
  :end-before: -- TEST_PREVIOUS_DOM_FROM_EOFEB_END

The :ref:`RollConvention <module-daml-finance-util-v4-date-rollconvention-61455>` functions
are often used in the context of rolling out a schedule, as
explained in the next section.

Schedule
========

The :ref:`Schedule <module-daml-finance-util-v4-date-schedule-63724>` module contains functions for
creating a series of time periods.
A schedule can be used to represent different aspects of a financial instrument, for example:

- payment periods, which define *when* a bond coupon or a swap payment should be paid
- calculation periods, which define *how* the interest amount is calculated

The following example defines a periodic 3M (3-month) schedule that rolls on the 30th of the month:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Schedule.daml
  :language: daml
  :start-after: -- CREATE_PERIODIC_SCHEDULE_BEGIN
  :end-before: -- CREATE_PERIODIC_SCHEDULE_END

Because there is one year between the start date and the end date, this periodic schedule should
correspond to four periods of 3M each:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Schedule.daml
  :language: daml
  :start-after: -- CREATE_EXPECTED_SCHEDULE_RESULT_BEGIN
  :end-before: -- CREATE_EXPECTED_SCHEDULE_RESULT_END

Note the distinction between adjusted and unadjusted dates. The unadjusted date is the result of
adding a specified time period to the start date of the previous schedule period. This date might
fall on a non-business day. Apply a
:ref:`BusinessDayAdjustment <type-daml-finance-interface-types-date-v3-calendar-businessdayadjustment-71551>`,
to get an adjusted date that falls on a business day.

Now you can roll out the schedule to get the specific start and end date for each period, and
compare the schedule to the expected result:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Schedule.daml
  :language: daml
  :start-after: -- CREATE_SCHEDULE_BEGIN
  :end-before: -- CREATE_SCHEDULE_END

Schedules often use the concept of *stub periods*. The above schedule had only
regular 3M periods. The following example reveals what happens if the schedule starts one month
later:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Schedule.daml
  :language: daml
  :start-after: -- CREATE_PERIODIC_SCHEDULE_WITH_STUB_BEGIN
  :end-before: -- CREATE_PERIODIC_SCHEDULE_WITH_STUB_END

This schedule has only 11 months in it, which does not correspond to regular 3M periods (neither 3
nor 4 periods).
To fix this, define a stub period.
Using the preceding example, you could define a start date for the first regular period. (This
implies a short initial stub period, in this case 2 months.) You would expect the following
schedule:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Schedule.daml
  :language: daml
  :start-after: -- CREATE_EXPECTED_SCHEDULE_RESULT_WITH_STUB_BEGIN
  :end-before: -- CREATE_EXPECTED_SCHEDULE_RESULT_WITH_STUB_END

To configure different stub types, use the
:ref:`StubPeriodTypeEnum <type-daml-finance-interface-types-date-v3-schedule-stubperiodtypeenum-99734>`
data type. You can configure whether the stub period should be at the beginning or end of the
schedule, as well as whether the stub period should be longer or shorter than a regular period.

Roll out this schedule, which includes a stub period, and compare it to the expected result:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/Schedule.daml
  :language: daml
  :start-after: -- CREATE_SCHEDULE_WITH_STUB_BEGIN
  :end-before: -- CREATE_SCHEDULE_WITH_STUB_END


DayCount
========

Many instruments that pay interest (often expressed as an annualized rate) require an exact
definition of how many days of interest belong to each payment period. The
:ref:`DayCount <module-daml-finance-util-v4-date-daycount-38488>` module provides functions  to support
such requirements.
In particular, you can calculate a *day count fraction* (*dcf*) between two dates. This indicates
the fraction of a full year between the dates.

For example, consider a one-year bond that pays a quarterly coupon according to a given schedule,
like the one described in the previous section.

This bond does not have the same number of days in each period, which normally results in different
coupon amounts for each period. Various market conventions address this and are provided in the
:ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`.

Consider the schedule with a short initial stub in the previous section. The following example uses
the ``Act360`` day count convention:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/DayCount.daml
  :language: daml
  :start-after: -- CALCULATE_ACT360_DCF_PERIOD1_BEGIN
  :end-before: -- CALCULATE_ACT360_DCF_PERIOD1_END

This corresponds to the 2M short initial stub period. The day count fraction can be used to
calculate a bond coupon or a swap payment for the given period, by multiplying it with the yearly
interest rate of the instrument (such as 4% per annum).

You can compare this initial day count fraction with those of the second and third periods of this
schedule, which are both regular (3M):

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/DayCount.daml
  :language: daml
  :start-after: -- CALCULATE_ACT360_DCF_PERIOD2AND3_BEGIN
  :end-before: -- CALCULATE_ACT360_DCF_PERIOD2AND3_END

These day count fractions are clearly greater than the one of the first period, since they
correspond to 3M instead of 2M. Note that they differ slightly: they do not contain the
exact same number of days.

In addition to ``Act360``, the
:ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
supports several other day count conventions. You can compute some of them with
:ref:`calcDcf <function-daml-finance-util-v4-date-daycount-calcdcf-53229>`, using only a start and an
end date as an input as shown above.
Others, such as ``ActActISDA``, are a bit more complicated because they require additional
information.
You can calculate them using the
:ref:`calcPeriodDcf <function-daml-finance-util-v4-date-daycount-calcperioddcf-52338>` function
instead. This function takes a schedule period (containing stub information) as input, as well as
the schedule end date and the payment frequency:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Util/Test/Date/DayCount.daml
  :language: daml
  :start-after: -- CALCULATE_ACTACTISDA_DCF_SCHEDULE_PERIOD_BEGIN
  :end-before: -- CALCULATE_ACTACTISDA_DCF_SCHEDULE_PERIOD_END

Summary
=======

You have learned about the date-related utility functions in the Daml Finance library.
Here are some pointers regarding next steps, depending on what you want to do:

- understand and validate the implementation of existing Daml Finance instruments: the
  implementation of the Bond package could be a good starting point. It is explained and linked
  in the
  :doc:`How to leverage Contingent Claims in custom instrument implementations <contingent-claims-instrument>`
  tutorial.
- develop your own instruments that depend on business day shifts, schedules, or day count
  conventions: the :doc:`Payoff Modelling <../../payoff-modeling/intro>` tutorials describe the
  Contingent Claims framework step by step. You can then create the dates for your instruments using
  the functions described in this tutorial.
