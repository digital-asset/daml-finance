.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-bond-v3-inflationlinked-instrument-99606:

Daml.Finance.Instrument.Bond.V3.InflationLinked.Instrument
==========================================================

Templates
---------

.. _type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121:

**template** `Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121_>`_

  This template models an inflation linked bond\.
  It pays an inflation adjusted coupon at the end of every coupon period\.
  The coupon is based on a fixed rate, which is applied to a principal that is adjusted according
  to an inflation index, for example the Consumer Price Index (CPI) in the U\.S\.
  For example\: 0\.5% p\.a\. coupon, CPI adjusted principal\:
  At maturity, the greater of the adjusted principal and the original principal is redeemed\.
  For clarity, this only applies to the redemption amount\. The coupons are always calculated based
  on the adjusted principal\.

  Signatory\: depository, issuer

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
       - :ref:`Id <type-daml-finance-interface-types-common-v3-types-id-28519>`
       - An identifier of the instrument\.
     * - version
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The instrument's version\.
     * - holdingStandard
       - :ref:`HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`
       - The holding standard for holdings referencing this instrument\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A description of the instrument\.
     * - inflationIndexId
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The inflation index reference ID\. For example, in case of \"0\.5% p\.a\. coupon, CPI adjusted principal\" this should be a valid reference to the \"CPI\" index\.
     * - inflationIndexBaseValue
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The value of the inflation index on the first reference date of this bond (called \"dated date\" on US TIPS)\. This is used as the base value for the principal adjustment\.
     * - couponRate
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The fixed coupon rate, per annum\. For example, in case of a \"0\.5% p\.a\. coupon, CPI adjusted principal\" this should be 0\.005\.
     * - periodicSchedule
       - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
       - The schedule for the periodic coupon payments\.
     * - holidayCalendarIds
       - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
       - The identifiers of the holiday calendars to be used for the coupon schedule\.
     * - calendarDataProvider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The reference data provider to use for the holiday calendar\.
     * - dayCountConvention
       - :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
       - The day count convention used to calculate day count fractions\. For example\: Act360\.
     * - currency
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - The currency of the bond\. For example, if the bond pays in USD this should be a USD cash instrument\.
     * - notional
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The notional of the bond\. This is the face value corresponding to one unit of the bond instrument\. For example, if one bond unit corresponds to 1000 USD, this should be 1000\.0\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The observers of the instrument\.
     * - lastEventTimestamp
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.

  + **Choice** Archive

    Controller\: depository, issuer

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466>` **for** `Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-bond-v3-inflationlinked-instrument-i-26275>` **for** `Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121_>`_

Data Types
----------

.. _type-daml-finance-instrument-bond-v3-inflationlinked-instrument-t-70415:

**type** `T <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-t-70415_>`_
  \= `Instrument <type-daml-finance-instrument-bond-v3-inflationlinked-instrument-instrument-42121_>`_

  Type synonym for ``Instrument``\.
