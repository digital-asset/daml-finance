.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-floatingrate-13055:

Module Daml.Finance.Interface.Instrument.Bond.FloatingRate
===============================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-floatingrate-factory-88424:

**interface** `Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424_>`_

  Interface that allows implementing templates to create instruments\.

  + **Choice Create**

    Create a new account\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
         - The instrument's key\.
       * - referenceRateId
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The floating rate reference ID\. For example, in case of \"3M Euribor \+ 0\.5%\" this should a valid reference to the \"3M Euribor\" reference rate\.
       * - couponSpread
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The floating rate coupon spread\. For example, in case of \"3M Euribor \+ 0\.5%\" this should be 0\.005\.
       * - issueDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The date when the bond was issued\.
       * - firstCouponDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The first coupon date of the bond\.
       * - maturityDate
         - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
         - The last coupon date (and the redemption date) of the bond\.
       * - holidayCalendarIds
         - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
         - the identifier of the holiday calendar to be used for the coupon schedule\.
       * - calendarDataProvider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The reference data provider to use for the holiday calendar\.
       * - dayCountConvention
         - :ref:`DayCountConventionEnum <type-daml-finance-common-date-daycount-daycountconventionenum-57741>`
         - The day count convention used to calculate day count fractions\. For example\: Act360\.
       * - businessDayConvention
         - :ref:`BusinessDayConventionEnum <type-daml-finance-common-date-calendar-businessdayconventionenum-67582>`
         - An enum type to specify how a non\-business day is adjusted\. For example\: FOLLOWING\.
       * - couponPeriod
         - :ref:`PeriodEnum <type-daml-finance-common-date-rollconvention-periodenum-40915>`
         - The coupon period\. For example, in case of a 3M coupon period (a coupon every 3 months), this should be M\.
       * - couponPeriodMultiplier
         - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
         - The coupon period multiplier\. For example, in case of a 3M coupon period (a coupon every 3 months), this should be 3\.
       * - currency
         - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-75164>`
         - The currency of the bond\. For example, if the bond pays in USD this should be a USD cash instrument\.
       * - lastEventTimestamp
         - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
         - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
       * - observers
         - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
         - The instrument's observers\.

  + **Choice Remove**

    Archive an account\.

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - instrument
         - :ref:`InstrumentKey <type-daml-finance-interface-asset-types-instrumentkey-68480>`
         - The account's key\.

  + **Method asDisclosure \:**\ :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

    Conversion to ``Disclosure`` interface\.

  + **Method create' \:**\ Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-66474>`)

    Implementation of ``Create`` choice\.

  + **Method remove \:**\ Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()

    Implementation of ``Remove`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-instrument-bond-floatingrate-hasimplementation-64293:

**class** `Implementation <type-daml-finance-interface-instrument-bond-floatingrate-implementation-45853_>`_ t \=\> `HasImplementation <class-daml-finance-interface-instrument-bond-floatingrate-hasimplementation-64293_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-instrument-bond-floatingrate-hasimplementation-64293_>`_ :ref:`Factory <type-daml-finance-instrument-bond-floatingrate-factory-2361>`

  **instance** `HasImplementation <class-daml-finance-interface-instrument-bond-floatingrate-hasimplementation-64293_>`_ `Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424_>`_

Data Types
----------

.. _type-daml-finance-interface-instrument-bond-floatingrate-f-81578:

**type** `F <type-daml-finance-interface-instrument-bond-floatingrate-f-81578_>`_
  \= `Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-bond-floatingrate-implementation-45853:

**type** `Implementation <type-daml-finance-interface-instrument-bond-floatingrate-implementation-45853_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)

  Type constraint used to require templates implementing ``Factory`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-instrument-bond-floatingrate-view-34838:

**data** `View <type-daml-finance-interface-instrument-bond-floatingrate-view-34838_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-bond-floatingrate-view-35113:

  `View <constr-daml-finance-interface-instrument-bond-floatingrate-view-35113_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-floatingrate-view-34838_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-instrument-bond-floatingrate-view-34838_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-floatingrate-view-34838_>`_

  **instance** HasInterfaceView `Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424_>`_ `View <type-daml-finance-interface-instrument-bond-floatingrate-view-34838_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-bond-floatingrate-asdisclosure-90658:

`asDisclosure <function-daml-finance-interface-instrument-bond-floatingrate-asdisclosure-90658_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-instrument-bond-floatingrate-createtick-693:

`create' <function-daml-finance-interface-instrument-bond-floatingrate-createtick-693_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424_>`_ \=\> t \-\> Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-66474>`)

.. _function-daml-finance-interface-instrument-bond-floatingrate-remove-1423:

`remove <function-daml-finance-interface-instrument-bond-floatingrate-remove-1423_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-bond-floatingrate-factory-88424_>`_ \=\> t \-\> Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
