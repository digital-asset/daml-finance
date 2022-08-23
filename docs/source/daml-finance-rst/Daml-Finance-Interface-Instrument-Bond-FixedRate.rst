.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-interface-instrument-bond-fixedrate-31328:

Module Daml.Finance.Interface.Instrument.Bond.FixedRate
=======================================================

Interfaces
----------

.. _type-daml-finance-interface-instrument-bond-fixedrate-factory-27717:

**interface** `Factory <type-daml-finance-interface-instrument-bond-fixedrate-factory-27717_>`_

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
         - :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>`
         - The instrument's key\.
       * - couponRate
         - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
         - The fixed coupon rate, per annum\. For example, in case of a \"3\.5% p\.a\. coupon\" this should be 0\.035\.
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
         - :ref:`K <type-daml-finance-interface-instrument-base-instrument-k-58546>`
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
         - :ref:`InstrumentKey <type-daml-finance-interface-common-types-instrumentkey-87168>`
         - The account's key\.
  
  + **Method asDisclosure \:** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`
    
    Conversion to ``Disclosure`` interface\.
  
  + **Method create' \:** Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-67236>`)
    
    Implementation of ``Create`` choice\.
  
  + **Method remove \:** Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
    
    Implementation of ``Remove`` choice\.

Typeclasses
-----------

.. _class-daml-finance-interface-instrument-bond-fixedrate-hasimplementation-40052:

**class** `Implementation <type-daml-finance-interface-instrument-bond-fixedrate-implementation-60718_>`_ t \=\> `HasImplementation <class-daml-finance-interface-instrument-bond-fixedrate-hasimplementation-40052_>`_ t **where**


Data Types
----------

.. _type-daml-finance-interface-instrument-bond-fixedrate-f-7211:

**type** `F <type-daml-finance-interface-instrument-bond-fixedrate-f-7211_>`_
  \= `Factory <type-daml-finance-interface-instrument-bond-fixedrate-factory-27717_>`_
  
  Type synonym for ``Factory``\.

.. _type-daml-finance-interface-instrument-bond-fixedrate-implementation-60718:

**type** `Implementation <type-daml-finance-interface-instrument-bond-fixedrate-implementation-60718_>`_ t
  \= (`HasToInterface <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-interface-hastointerface-68104>`_ t `Factory <type-daml-finance-interface-instrument-bond-fixedrate-factory-27717_>`_, :ref:`Implementation <type-daml-finance-interface-common-disclosure-implementation-6532>` t)
  
  Type constraint used to require templates implementing ``Factory`` to also
  implement ``Disclosure``\.

.. _type-daml-finance-interface-instrument-bond-fixedrate-view-67297:

**data** `View <type-daml-finance-interface-instrument-bond-fixedrate-view-67297_>`_

  View of ``Factory``\.
  
  .. _constr-daml-finance-interface-instrument-bond-fixedrate-view-42382:
  
  `View <constr-daml-finance-interface-instrument-bond-fixedrate-view-42382_>`_
  
    .. list-table::
       :widths: 15 10 30
       :header-rows: 1
    
       * - Field
         - Type
         - Description
       * - provider
         - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
         - The provider of the ``Factory``\.
  
  **instance** `Eq <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-eq-22713>`_ `View <type-daml-finance-interface-instrument-bond-fixedrate-view-67297_>`_
  
  **instance** `Ord <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-classes-ord-6395>`_ `View <type-daml-finance-interface-instrument-bond-fixedrate-view-67297_>`_
  
  **instance** `Show <https://docs.daml.com/daml/stdlib/Prelude.html#class-ghc-show-show-65360>`_ `View <type-daml-finance-interface-instrument-bond-fixedrate-view-67297_>`_

Functions
---------

.. _function-daml-finance-interface-instrument-bond-fixedrate-asdisclosure-55581:

`asDisclosure <function-daml-finance-interface-instrument-bond-fixedrate-asdisclosure-55581_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-bond-fixedrate-factory-27717_>`_ \=\> t \-\> :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _function-daml-finance-interface-instrument-bond-fixedrate-createtick-72876:

`create' <function-daml-finance-interface-instrument-bond-fixedrate-createtick-72876_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-bond-fixedrate-factory-27717_>`_ \=\> t \-\> Create \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-instrument-base-instrument-i-67236>`)

.. _function-daml-finance-interface-instrument-bond-fixedrate-remove-61536:

`remove <function-daml-finance-interface-instrument-bond-fixedrate-remove-61536_>`_
  \: `Implements <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-interface-implements-92077>`_ t `Factory <type-daml-finance-interface-instrument-bond-fixedrate-factory-27717_>`_ \=\> t \-\> Remove \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ ()
