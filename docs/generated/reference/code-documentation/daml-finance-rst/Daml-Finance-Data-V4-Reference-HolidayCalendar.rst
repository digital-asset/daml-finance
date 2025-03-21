.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-data-v4-reference-holidaycalendar-15110:

Daml.Finance.Data.V4.Reference.HolidayCalendar
==============================================

Templates
---------

.. _type-daml-finance-data-v4-reference-holidaycalendar-factory-82307:

**template** `Factory <type-daml-finance-data-v4-reference-holidaycalendar-factory-82307_>`_

  Implementation of the corresponding HolidayCalendar Factory\.

  Signatory\: provider

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The factory's provider\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The factory's observers\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-i-75115>` **for** `Factory <type-daml-finance-data-v4-reference-holidaycalendar-factory-82307_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Factory <type-daml-finance-data-v4-reference-holidaycalendar-factory-82307_>`_

.. _type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871:

**template** `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_

  Holiday calendar of an entity (typically an exchange or a currency)\.
  It is maintained by a reference data provider\.

  Signatory\: provider

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - calendar
       - :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`
       - Holiday Calendar Data used to define holidays\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - Observers\.
     * - provider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The party maintaining the ``HolidayCalendar``\.

  + **Choice** Archive

    Controller\: provider

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-data-v4-reference-holidaycalendar-getcalendar-25827:

    **Choice** `GetCalendar <type-daml-finance-data-v4-reference-holidaycalendar-getcalendar-25827_>`_

    Returns the calendar's ``HolidayCalendarData``\.

    Controller\: viewer

    Returns\: :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party fetching the calendar\.

  + **interface instance** :ref:`I <type-daml-finance-interface-data-v4-reference-holidaycalendar-i-49491>` **for** `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_

Data Types
----------

.. _type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417:

**data** `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_

  Key used to look up the holiday calendar of an entity, as defined by a reference data provider\.

  .. _constr-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-7166:

  `HolidayCalendarKey <constr-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-7166_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party maintaining the ``HolidayCalendar``\.
       * - id
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A textual label identifying the calendar (e\.g\. \"NYSE\" for the New York Stock Exchange holiday calendar)\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_ `GetCalendar <type-daml-finance-data-v4-reference-holidaycalendar-getcalendar-25827_>`_ :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`

  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()

  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_

  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_

  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_

  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_

  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_

  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ `HolidayCalendar <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendar-24871_>`_ `HolidayCalendarKey <type-daml-finance-data-v4-reference-holidaycalendar-holidaycalendarkey-90417_>`_

Functions
---------

.. _function-daml-finance-data-v4-reference-holidaycalendar-getholidaycalendars-73504:

`getHolidayCalendars <function-daml-finance-data-v4-reference-holidaycalendar-getholidaycalendars-73504_>`_
  \: `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`\]

  Retrieve holiday calendar(s) from the ledger\.

.. _function-daml-finance-data-v4-reference-holidaycalendar-rollschedule-90182:

`rollSchedule <function-daml-finance-data-v4-reference-holidaycalendar-rollschedule-90182_>`_
  \: (\[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`\]) \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (:ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>`, \[:ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`\])

  Retrieve holiday calendar(s) from the ledger and roll out a schedule\.
  Returns the rolled schedule and the required calendars\.
