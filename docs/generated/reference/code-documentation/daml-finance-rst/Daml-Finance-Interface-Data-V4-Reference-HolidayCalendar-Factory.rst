.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-data-v4-reference-holidaycalendar-factory-36485:

Daml.Finance.Interface.Data.V4.Reference.HolidayCalendar.Factory
================================================================

Interfaces
----------

.. _type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-factory-89386:

**interface** `Factory <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-factory-89386_>`_

  Interface that allows implementing templates to create holiday calendars\.

  **viewtype** `V <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-v-94828_>`_

  + **Choice** Archive

    Controller\: Signatories of implementing template

    Returns\: ()

    (no fields)

  + .. _type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-create-25637:

    **Choice** `Create <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-create-25637_>`_

    Create a new Holiday Calendar\.

    Controller\: provider

    Returns\: `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-data-v4-reference-holidaycalendar-i-49491>`

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
         - The calendar's provider\.

  + **Method create' \:** `Create <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-create-25637_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-data-v4-reference-holidaycalendar-i-49491>`)

    Implementation of ``Create`` choice\.

Data Types
----------

.. _type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-f-64348:

**type** `F <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-f-64348_>`_
  \= `Factory <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-factory-89386_>`_

.. _type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-i-75115:

**type** `I <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-i-75115_>`_
  \= `Factory <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-factory-89386_>`_

  Type synonyms for ``Factory``\.

.. _type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-v-94828:

**type** `V <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-v-94828_>`_
  \= `View <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-view-44824_>`_

  Type synonym for ``View``\.

  **instance** `HasFromAnyView <https://docs.daml.com/daml/stdlib/DA-Internal-Interface-AnyView.html#class-da-internal-interface-anyview-hasfromanyview-30108>`_ `Factory <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-factory-89386_>`_ `V <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-v-94828_>`_

.. _type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-view-44824:

**data** `View <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-view-44824_>`_

  .. _constr-daml-finance-interface-data-v4-reference-holidaycalendar-factory-view-427:

  `View <constr-daml-finance-interface-data-v4-reference-holidaycalendar-factory-view-427_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-view-44824_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-view-44824_>`_

Functions
---------

.. _function-daml-finance-interface-data-v4-reference-holidaycalendar-factory-createtick-22339:

`create' <function-daml-finance-interface-data-v4-reference-holidaycalendar-factory-createtick-22339_>`_
  \: `Factory <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-factory-89386_>`_ \-\> `Create <type-daml-finance-interface-data-v4-reference-holidaycalendar-factory-create-25637_>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-data-v4-reference-holidaycalendar-i-49491>`)
