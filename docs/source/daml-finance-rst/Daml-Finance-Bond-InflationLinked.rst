.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-bond-inflationlinked-4596:

Module Daml.Finance.Bond.InflationLinked
========================================

Templates
---------

.. _type-daml-finance-bond-inflationlinked-factory-12125:

**template** `Factory <type-daml-finance-bond-inflationlinked-factory-12125_>`_

  Factory template for instrument creation\.

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
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - The factory's observers\.

  + **Choice Archive**


  + **implements** :ref:`Factory <type-daml-finance-interface-bond-inflationlinked-factory-99998>`

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

.. _type-daml-finance-bond-inflationlinked-instrument-28311:

**template** `Instrument <type-daml-finance-bond-inflationlinked-instrument-28311_>`_

  This template models an inflation linked bond\.
  It pays a fixed coupon rate at the end of every coupon period\.
  This is calculated based on a principal that is adjusted according to an inflation index, for example the Consumer Price Index (CPI) in the U\.S\.
  For example\: 0\.5% p\.a coupon, CPI adjusted principal\:
  At maturity, the greater of the adjusted principal and the original principal is redeemed\.
  For clarity, this only applies to the redemption amount\. The coupons are always calculated based on the adjusted principal\.

  .. list-table::
     :widths: 15 10 30
     :header-rows: 1

     * - Field
       - Type
       - Description
     * - depository
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The depository of the instrument\.
     * - issuer
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The issuer of the instrument\.
     * - id
       - :ref:`Id <type-daml-finance-interface-asset-types-id-89116>`
       - An identifier of the instrument\.
     * - inflationIndexId
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The inflation index reference ID\. For example, in case of \"0\.5% p\.a coupon, CPI adjusted principal\" this should a valid reference to the \"CPI\" index\.
     * - inflationIndexBaseValue
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The value of the inflation index on the first reference date of this bond (called \"dated date\" on US TIPS)\. This is used as the base value for the principal adjustment\.
     * - couponRate
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The fixed coupon rate, per annum\. For example, in case of a \"0\.5% p\.a coupon, CPI adjusted principal\" this should be 0\.005\.
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
       - :ref:`K <type-daml-finance-interface-asset-instrument-k-75164>`
       - The currency of the bond\. For example, if the bond pays in USD this should be a USD cash instrument\.
     * - observers
       - :ref:`Observers <type-daml-finance-interface-common-types-observers-20361>`
       - The observers of the instrument\.
     * - lastEventTimestamp
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - (market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  + **Choice Archive**


  + **implements** :ref:`I <type-daml-finance-interface-asset-instrument-i-66474>`

  + **implements** :ref:`I <type-daml-finance-interface-common-disclosure-i-70158>`

  + **implements** :ref:`I <type-daml-finance-interface-generic-hasclaims-i-90893>`

  + **implements** :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`

Data Types
----------

.. _type-daml-finance-bond-inflationlinked-t-96325:

**type** `T <type-daml-finance-bond-inflationlinked-t-96325_>`_
  \= `Instrument <type-daml-finance-bond-inflationlinked-instrument-28311_>`_

  **instance** :ref:`HasImplementation <class-daml-finance-interface-asset-instrument-hasimplementation-51108>` `T <type-daml-finance-bond-inflationlinked-t-96325_>`_
