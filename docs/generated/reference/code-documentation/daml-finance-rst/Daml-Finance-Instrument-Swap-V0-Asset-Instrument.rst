.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-swap-v0-asset-instrument-74976:

Daml.Finance.Instrument.Swap.V0.Asset.Instrument
================================================

Templates
---------

.. _type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627:

**template** `Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627_>`_

  This template models an asset swap\.
  It pays an asset performance vs a fix interest rate at the end of every payment period\.
  It can be used to model equity swaps, some types of commodity swaps (of the form performance vs
  rate) and swaps with the same payoff on other asset types\.
  The asset leg is described by an observable containing either unadjusted or adjusted fixings (for
  a price return or a total return swap, respectively)\.
  The template calculates the performance for each payment period using this observable\.
  For example\: AAPL total return vs 2\.5% fix\.

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
     * - underlyings
       - \[:ref:`Underlying <type-daml-finance-interface-instrument-swap-v0-asset-types-underlying-93813>`\]
       - The list of underlyings (the basket of reference assets for the asset swap)\.
     * - ownerReceivesRate
       - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
       - Indicate whether a holding owner of this instrument receives the rate or the asset leg of the swap\.
     * - floatingRate
       - `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149>`
       - A description of the floating rate to be used (if applicable)\. This supports both Libor and SOFR style reference rates (using a compounded index, e\.g\. the SOFR Index)\.
     * - fixRate
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The interest rate of the fix leg\. For example, in case of \"AAPL total return vs 2\.5% fix\" this should be 0\.025\. This can also be used as a floating rate spread\. For example, in case of \"3M Libor \+ 0\.5%\" this should be 0\.005\.
     * - periodicSchedule
       - :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`
       - The schedule for the periodic swap payments\.
     * - holidayCalendarIds
       - \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\]
       - The identifiers of the holiday calendars to be used for the swap payment schedule\.
     * - calendarDataProvider
       - `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_
       - The reference data provider to use for the holiday calendar\.
     * - dayCountConvention
       - :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
       - The day count convention used to calculate day count fractions\. For example\: Act360\.
     * - currency
       - :ref:`InstrumentKey <type-daml-finance-interface-types-common-v3-types-instrumentkey-82717>`
       - The currency of the swap\. For example, if the swap pays in USD this should be a USD cash instrument\.
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

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466>` **for** `Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-swap-v0-asset-instrument-i-95573>` **for** `Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627_>`_

Data Types
----------

.. _type-daml-finance-instrument-swap-v0-asset-instrument-t-64265:

**type** `T <type-daml-finance-instrument-swap-v0-asset-instrument-t-64265_>`_
  \= `Instrument <type-daml-finance-instrument-swap-v0-asset-instrument-instrument-26627_>`_

  Type synonym for ``Instrument``\.
