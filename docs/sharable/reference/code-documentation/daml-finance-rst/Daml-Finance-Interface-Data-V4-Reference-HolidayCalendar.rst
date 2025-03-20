.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-data-v4-reference-holidaycalendar-24201:

Daml.Finance.Interface.Data.V4.Reference.HolidayCalendar
========================================================

Interfaces
----------

.. _type-daml-finance-interface-data-v4-reference-holidaycalendar-holidaycalendar-62534:

**interface** `HolidayCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-holidaycalendar-62534_>`_

  Interface for contracts storing holiday calendar data on the ledger\.

  **viewtype** `V <type-daml-finance-interface-data-v4-reference-holidaycalendar-v-77764_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-data-v4-reference-holidaycalendar-getview-27451:

    **Choice** `GetView <type-daml-finance-interface-data-v4-reference-holidaycalendar-getview-27451_>`_

    Retrieves the interface view\.

    Controller\: viewer

    Returns\: `View <type-daml-finance-interface-data-v4-reference-holidaycalendar-view-55728_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - viewer
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The party fetching the view\.

  + .. _type-daml-finance-interface-data-v4-reference-holidaycalendar-updatecalendar-40272:

    **Choice** `UpdateCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-updatecalendar-40272_>`_

    Updates the holiday calendar\.

    Controller\: (DA\.Internal\.Record\.getField @\"provider\" (view this))

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `HolidayCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-holidaycalendar-62534_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - newCalendar
         - :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`
         - The new ``HolidayCalendarData``\.

  + **Method updateCalendar \:** `UpdateCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-updatecalendar-40272_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `HolidayCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-holidaycalendar-62534_>`_)

    Updates the holiday calendar\.

Data Types
----------

.. _type-daml-finance-interface-data-v4-reference-holidaycalendar-i-49491:

**type** `I <type-daml-finance-interface-data-v4-reference-holidaycalendar-i-49491_>`_
  \= `HolidayCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-holidaycalendar-62534_>`_

  Type synonym for ``HolidayCalendar``\.

  **instance** HasMethod :ref:`Factory <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-factory-89386>` \"create'\" (:ref:`Create <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-create-25637>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `I <type-daml-finance-interface-data-v4-reference-holidaycalendar-i-49491_>`_))

.. _type-daml-finance-interface-data-v4-reference-holidaycalendar-v-77764:

**type** `V <type-daml-finance-interface-data-v4-reference-holidaycalendar-v-77764_>`_
  \= `View <type-daml-finance-interface-data-v4-reference-holidaycalendar-view-55728_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `HolidayCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-holidaycalendar-62534_>`_ `V <type-daml-finance-interface-data-v4-reference-holidaycalendar-v-77764_>`_

.. _type-daml-finance-interface-data-v4-reference-holidaycalendar-view-55728:

**data** `View <type-daml-finance-interface-data-v4-reference-holidaycalendar-view-55728_>`_

  View for ``HolidayCalendar``\.

  .. _constr-daml-finance-interface-data-v4-reference-holidaycalendar-view-52807:

  `View <constr-daml-finance-interface-data-v4-reference-holidaycalendar-view-52807_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The parties providing the ``HolidayCalendar``\.
       * - calendar
         - :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`
         - Holiday Calendar Data used to define holidays\.

Functions
---------

.. _function-daml-finance-interface-data-v4-reference-holidaycalendar-updatecalendar-64684:

`updateCalendar <function-daml-finance-interface-data-v4-reference-holidaycalendar-updatecalendar-64684_>`_
  \: `HolidayCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-holidaycalendar-62534_>`_ \-\> `UpdateCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-updatecalendar-40272_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ `HolidayCalendar <type-daml-finance-interface-data-v4-reference-holidaycalendar-holidaycalendar-62534_>`_)
