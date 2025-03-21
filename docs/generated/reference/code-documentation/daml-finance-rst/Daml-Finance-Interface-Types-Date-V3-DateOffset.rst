.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-types-date-v3-dateoffset-17578:

Daml.Finance.Interface.Types.Date.V3.DateOffset
===============================================

Data Types
----------

.. _type-daml-finance-interface-types-date-v3-dateoffset-dateoffset-75159:

**data** `DateOffset <type-daml-finance-interface-types-date-v3-dateoffset-dateoffset-75159_>`_

  A date offset type that can be used e\.g\. to specify a rate fixing date relative to the reset
  date in terms of a business days offset and an associated set of financial business centers\.

  .. _constr-daml-finance-interface-types-date-v3-dateoffset-dateoffset-94650:

  `DateOffset <constr-daml-finance-interface-types-date-v3-dateoffset-dateoffset-94650_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - period
         - :ref:`PeriodEnum <type-daml-finance-interface-types-date-v3-rollconvention-periodenum-45289>`
         - The unit of the date offset, e\.g\. D means that the date offset is specified in days\.
       * - periodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         - The number of days (if period is D) before or after the base date the fixing is observed\.
       * - dayType
         - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `DayTypeEnum <type-daml-finance-interface-types-date-v3-dateoffset-daytypeenum-48232_>`_
         - Indicate whether the date offset is given in Business days or Calendar days\.
       * - businessDayConvention
         - :ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>`
         - Business day convention that describes how a non\-business day is adjusted\.
       * - businessCenters
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         - The identifiers of the holiday calendars to be used for date adjustment (if any)\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `DateOffset <type-daml-finance-interface-types-date-v3-dateoffset-dateoffset-75159_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `DateOffset <type-daml-finance-interface-types-date-v3-dateoffset-dateoffset-75159_>`_

.. _type-daml-finance-interface-types-date-v3-dateoffset-daytypeenum-48232:

**data** `DayTypeEnum <type-daml-finance-interface-types-date-v3-dateoffset-daytypeenum-48232_>`_

  A day type classification used in counting the number of days between two dates\.

  .. _constr-daml-finance-interface-types-date-v3-dateoffset-business-5407:

  `Business <constr-daml-finance-interface-types-date-v3-dateoffset-business-5407_>`_

    When calculating the number of days between two dates the count includes only business
    days\.

  .. _constr-daml-finance-interface-types-date-v3-dateoffset-calendar-92195:

  `Calendar <constr-daml-finance-interface-types-date-v3-dateoffset-calendar-92195_>`_

    When calculating the number of days between two dates the count includes all calendar days\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `DayTypeEnum <type-daml-finance-interface-types-date-v3-dateoffset-daytypeenum-48232_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `DayTypeEnum <type-daml-finance-interface-types-date-v3-dateoffset-daytypeenum-48232_>`_
