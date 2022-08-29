.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-refdata-holidaycalendar-69798:

Module Daml.Finance.RefData.HolidayCalendar
===========================================

Templates
---------

.. _type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891:

**template** `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_

  Holiday calendar of an entity (typically an exchange or a currency)\.
  It is maintained by a reference data agency\.
  A public party is used as an observer in order to make this data publicly readable\.
  
  .. list-table::
     :widths: 15 10 30
     :header-rows: 1
  
     * - Field
       - Type
       - Description
     * - agency
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The party maintaining the ``HolidayCalendar``\.
     * - entity
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A textual label identifying the calendar (e\.g\. \"NYSE\" for the New York Stock Exchange holiday calendar)\.
     * - calendar
       - :ref:`HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016>`
       - Holiday Calendar Data used to define holidays\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - Observers\.
  
  + **Choice Archive**
    
  
  + **Choice GetCalendar**
    
    Returns the calendar's ``HolidayCalendarData``\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party fetching the calendar\.
  
  + **Choice UpdateCalendar**
    
    Updates the holiday calendar\.
    
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - newCalendar
         - :ref:`HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016>`
         - The new ``HolidayCalendarData``\.

Data Types
----------

.. _type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389:

**data** `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_

  Key used to look up the holiday calendar of an entity, as defined by a reference data agency\.
  
  .. _constr-daml-finance-refdata-holidaycalendar-holidaycalendarkey-67206:
  
  `HolidayCalendarKey <constr-daml-finance-refdata-holidaycalendar-holidaycalendarkey-67206_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - agency
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party maintaining the ``HolidayCalendar``\.
       * - entity
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - A textual label identifying the calendar (e\.g\. \"NYSE\" for the New York Stock Exchange holiday calendar)\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_ GetCalendar :ref:`HolidayCalendarData <type-daml-finance-common-date-calendar-holidaycalendardata-72016>`
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_ UpdateCalendar (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_)
  
  **instance** `HasExerciseByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasexercisebykey-36549>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_ `Archive <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-template-archive-15178>`_ ()
  
  **instance** `HasFetchByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfetchbykey-54638>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_
  
  **instance** `HasFromAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasfromanycontractkey-95587>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_
  
  **instance** `HasKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haskey-87616>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_
  
  **instance** `HasLookupByKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-haslookupbykey-92299>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_
  
  **instance** `HasMaintainer <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hasmaintainer-28932>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_
  
  **instance** `HasToAnyContractKey <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-template-functions-hastoanycontractkey-35010>`_ `HolidayCalendar <type-daml-finance-refdata-holidaycalendar-holidaycalendar-89891_>`_ `HolidayCalendarKey <type-daml-finance-refdata-holidaycalendar-holidaycalendarkey-14389_>`_
