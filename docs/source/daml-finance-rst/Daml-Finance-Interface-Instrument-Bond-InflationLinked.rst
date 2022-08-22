.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-inflationlinked-78161:

Module Daml.Finance.Interface.Instrument.Bond.InflationLinked
==================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998:

**interface** `Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998_>`_

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
       * - inflationIndexId
         - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
         - The inflation index reference ID\. For example, in case of \"0\.5% p\.a\. coupon, CPI adjusted principal\" this should a valid reference to the \"CPI\" index\.
       * - inflationIndexBaseValue
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The value of the inflation index on the first reference date of this bond (called \"dated date\" on US TIPS)\. This is used as the base value for the principal adjustment\.
       * - couponRate
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fixed coupon rate, per annum\. For example, in case of a \"0\.5% p\.a\. coupon, CPI adjusted principal\" this should be 0\.005\.
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

.. _class-daml-finance-interface-instrument-bond-inflationlinked-hasimplementation-3445:

**class** `Implementation <type-daml-finance-interface-instrument-bond-inflationlinked-implementation-7499_>`_ t \=\> `HasImplementation <class-daml-finance-interface-instrument-bond-inflationlinked-hasimplementation-3445_>`_ t **where**

  **instance** `HasImplementation <class-daml-finance-interface-instrument-bond-inflationlinked-hasimplementation-3445_>`_ :ref:`Factory <type-daml-finance-instrument-bond-inflationlinked-factory-12125>`

  **instance** `HasImplementation <class-daml-finance-interface-instrument-bond-inflationlinked-hasimplementation-3445_>`_ `Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998_>`_

Data Types
----------

.. _type-daml-finance-interface-instrument-bond-inflationlinked-f-38824:

**type** `F <type-daml-finance-interface-instrument-bond-inflationlinked-f-38824_>`_
  \= `Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998_>`_

  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-bond-inflationlinked-implementation-7499:

**type** `Implementation <type-daml-finance-interface-instrument-bond-inflationlinked-implementation-7499_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)

  Type constraint used to require templates implementing ``Factory`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-instrument-bond-inflationlinked-view-49812:

**data** `View <type-daml-finance-interface-instrument-bond-inflationlinked-view-49812_>`_

  View of ``Factory``\.

  .. _constr-daml-finance-interface-instrument-bond-inflationlinked-view-99885:

  `View <constr-daml-finance-interface-instrument-bond-inflationlinked-view-99885_>`_

    .. list-table::
       :widths: 15 10 30
       :header-rows: 1

       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.

  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-inflationlinked-view-49812_>`_

  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-instrument-bond-inflationlinked-view-49812_>`_

  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-inflationlinked-view-49812_>`_

  **instance** HasInterfaceView `Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998_>`_ `View <type-daml-finance-interface-instrument-bond-inflationlinked-view-49812_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-bond-inflationlinked-asdisclosure-8368:

`asDisclosure <function-daml-finance-interface-instrument-bond-inflationlinked-asdisclosure-8368_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-instrument-bond-inflationlinked-createtick-5607:

`create' <function-daml-finance-interface-instrument-bond-inflationlinked-createtick-5607_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998_>`_ \=\> t \-\> Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-66474>`)

.. _function-daml-finance-interface-instrument-bond-inflationlinked-remove-83245:

`remove <function-daml-finance-interface-instrument-bond-inflationlinked-remove-83245_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-bond-inflationlinked-factory-99998_>`_ \=\> t \-\> Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
