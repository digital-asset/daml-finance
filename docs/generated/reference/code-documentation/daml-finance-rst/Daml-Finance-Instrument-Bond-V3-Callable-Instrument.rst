.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-bond-v3-callable-instrument-35206:

Daml.Finance.Instrument.Bond.V3.Callable.Instrument
===================================================

Templates
---------

.. _type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277:

**template** `Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277_>`_

  This template models a callable bond\.
  It pays a fixed or a floating coupon rate at the end of every coupon period (unless the bond has
  been called)\.
  Callability is restricted to some (or all) of the coupon dates\. This is specified by a separate
  schedule\.

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
       - The identifier of the instrument\.
     * - version
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The instrument's version\.
     * - holdingStandard
       - :ref:`HoldingStandard <type-daml-finance-interface-types-common-v3-types-holdingstandard-63293>`
       - The holding standard for holdings referencing this instrument\.
     * - description
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - A description of the instrument\.
     * - floatingRate
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149>`
       - A description of the floating rate to be used (if applicable)\. This supports both Libor and SOFR style reference rates (using a compounded index, e\.g\. the SOFR Index)\.
     * - couponRate
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The fixed coupon rate, per annum\. For example, in case of a \"3\.5% p\.a coupon\" this should be 0\.035\. This can also be used as a floating coupon spread\. For example, in case of \"3M Libor \+ 0\.5%\" this should be 0\.005\.
     * - capRate
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The maximum coupon rate possible\. For example, if \"3M Libor \+ 0\.5%\" would result in a rate of 2\.5%, but capRate is 2\.0%, the coupon rate used would be 2\.0%\.
     * - floorRate
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The minimum coupon rate possible\. For example, if \"3M Libor \+ 0\.5%\" would result in a rate of \-0\.2%, but floorRate is 0\.0%, the coupon rate used would be 0\.0%\.
     * - couponSchedule
       - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
       - The schedule for the periodic coupon payments\. The coupon is paid on the *last* date of each schedule period\. In case of a floating rate, the reference rate will be fixed in relation to this schedule (in case of a Libor rate\: at the start/end of each period, as specified by FloatingRate)\. This is the main schedule of the instrument, which drives both the calculation and the payment of coupons\. It also defines the issue date and the maturity date of the bond\.
     * - callSchedule
       - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
       - The bond is callable on the *last* date of each schedule period\. For example, if this schedule is the same as the couponSchedule, it means that the bond can be called on each coupon payment date\.
     * - noticeDays
       - `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_
       - The number of business days in advance of the coupon date that the issuer must give notice if it wants to call the bond\. The election whether to call or not to call must be done by this date\.
     * - holidayCalendarIds
       - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
       - The identifiers of the holiday calendars to be used for the coupon schedule\.
     * - calendarDataProvider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The reference data provider to use for the holiday calendar\.
     * - dayCountConvention
       - :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
       - The day count convention used to calculate day count fractions\. For example\: Act360\.
     * - useAdjustedDatesForDcf
       - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
       - Configure whether to use adjusted dates (as specified in *businessDayAdjustment* of the *couponSchedule*) for day count fractions\.
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
     * - prevEvents
       - \[EventData\]
       - A list of previous events that have been lifecycled on this instrument so far\. CALLABLE\_BOND\_TEMPLATE\_END

  + **Choice** Archive

    Controller\: depository, issuer

    Returns\: ()

    (no fields)

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466>` **for** `Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-bond-v3-callable-instrument-i-23721>` **for** `Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277_>`_

Data Types
----------

.. _type-daml-finance-instrument-bond-v3-callable-instrument-t-95027:

**type** `T <type-daml-finance-instrument-bond-v3-callable-instrument-t-95027_>`_
  \= `Instrument <type-daml-finance-instrument-bond-v3-callable-instrument-instrument-58277_>`_

  Type synonym for ``Instrument``\.
