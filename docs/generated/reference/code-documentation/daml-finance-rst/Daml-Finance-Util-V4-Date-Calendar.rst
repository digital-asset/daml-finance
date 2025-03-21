.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-util-v4-date-calendar-71711:

Daml.Finance.Util.V4.Date.Calendar
==================================

Functions
---------

.. _function-daml-finance-util-v4-date-calendar-merge-42976:

`merge <function-daml-finance-util-v4-date-calendar-merge-42976_>`_
  \: \[:ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`\] \-\> :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`

  Merge multiple holiday calendars into a single one\. ``id``\\s are concatenated by ``,``\.

.. _function-daml-finance-util-v4-date-calendar-isholiday-68144:

`isHoliday <function-daml-finance-util-v4-date-calendar-isholiday-68144_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Check if Date is a holiday\.

.. _function-daml-finance-util-v4-date-calendar-isbusinessday-69488:

`isBusinessDay <function-daml-finance-util-v4-date-calendar-isbusinessday-69488_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_

  Check if Date is a business day\.

.. _function-daml-finance-util-v4-date-calendar-nextbusinessday-34187:

`nextBusinessDay <function-daml-finance-util-v4-date-calendar-nextbusinessday-34187_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Get next business day\.

.. _function-daml-finance-util-v4-date-calendar-previousbusinessday-90099:

`previousBusinessDay <function-daml-finance-util-v4-date-calendar-previousbusinessday-90099_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Get previous business day\.

.. _function-daml-finance-util-v4-date-calendar-nextorsamebusinessday-87326:

`nextOrSameBusinessDay <function-daml-finance-util-v4-date-calendar-nextorsamebusinessday-87326_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Get next or same business day\.

.. _function-daml-finance-util-v4-date-calendar-previousorsamebusinessday-55758:

`previousOrSameBusinessDay <function-daml-finance-util-v4-date-calendar-previousorsamebusinessday-55758_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Get previous or same business day\.

.. _function-daml-finance-util-v4-date-calendar-nextsameorlastinmonthbusinessday-70564:

`nextSameOrLastInMonthBusinessDay <function-daml-finance-util-v4-date-calendar-nextsameorlastinmonthbusinessday-70564_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Get next or same business day if before end of month\. Otherwise get last business day in month\.

.. _function-daml-finance-util-v4-date-calendar-previoussameorfirstinmonthbusinessday-92367:

`previousSameOrFirstInMonthBusinessDay <function-daml-finance-util-v4-date-calendar-previoussameorfirstinmonthbusinessday-92367_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Get previous or same business day if before end of month\. Otherwise get first business day in
  month\.

.. _function-daml-finance-util-v4-date-calendar-addbusinessdays-98940:

`addBusinessDays <function-daml-finance-util-v4-date-calendar-addbusinessdays-98940_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Add business days to a Date\.

.. _function-daml-finance-util-v4-date-calendar-adjustdate-36378:

`adjustDate <function-daml-finance-util-v4-date-calendar-adjustdate-36378_>`_
  \: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> :ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Adjust date according to the given business day convention\.
