.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-60122:

Daml.Finance.Instrument.StructuredProduct.V0.BarrierReverseConvertible.Instrument
=================================================================================

Templates
---------

.. _type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873:

**template** `Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873_>`_

  This template models a Barrier Reverse Convertible (BRC) instrument\.
  It can be seen as a long fixed coupon bond and a short Down\-And\-In put option\.

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
     * - referenceAssetId
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The reference asset ID\. For example, in case of an option on AAPL this should be a valid reference to the AAPL fixings to be used for the payoff calculation\.
     * - strike
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The strike price of the option\.
     * - barrier
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The barrier level of the option\.
     * - barrierStartDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The start date for barrier observations\.
     * - expiryDate
       - `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_
       - The expiry date of the instrument\.
     * - couponRate
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The fixed coupon rate, per annum\. For example, in case of a \"3\.5% p\.a coupon\" this should be 0\.035\.
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
       - The currency of the product\. For example, if the product pays in USD this should be a USD cash instrument\.
     * - notional
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The notional of the product\. This is the face value corresponding to one unit of the product\. For example, if one product unit corresponds to 1000 USD, this should be 1000\.0\.
     * - observers
       - :ref:`PartiesMap <type-daml-finance-interface-types-common-v3-types-partiesmap-43006>`
       - The observers of the instrument\.
     * - lastEventTimestamp
       - `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_
       - (Market) time of the last recorded lifecycle event\. If no event has occurred yet, the time of creation should be used\.
     * - prevEvents
       - \[EventData\]
       - A list of previous events that have been lifecycled on this instrument so far\.

  + **Choice** Archive

    Controller\: depository, issuer

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466>` **for** `Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-i-56857>` **for** `Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873_>`_

Data Types
----------

.. _type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-t-60935:

**type** `T <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-t-60935_>`_
  \= `Instrument <type-daml-finance-instrument-structuredproduct-v0-barrierreverseconvertible-instrument-instrument-83873_>`_

  Type synonym for ``Instrument``\.
