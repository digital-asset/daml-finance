.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-swap-v0-creditdefault-instrument-93234:

Daml.Finance.Instrument.Swap.V0.CreditDefault.Instrument
========================================================

Templates
---------

.. _type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085:

**template** `Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085_>`_

  This template models a cash\-settled credit default swap\.
  In case of a credit default event it pays (1\-recoveryRate), in exchange for a fix rate at the end
  of every payment period\. For example\: 2\.5% fix vs (1\-recoveryRate) if TSLA defaults on a bond
  payment

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
     * - defaultProbabilityReferenceId
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The reference ID of the default probability observable\. For example, in case of protection against a \"TSLA bond payment default\" this should be a valid reference to the \"TSLA default probability\"\.
     * - recoveryRateReferenceId
       - `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_
       - The reference ID of the recovery rate observable\. For example, in case of a \"TSLA bond payment default with a 60% recovery rate\" this should be a valid reference to the \"TSLA bond recovery rate\"\.
     * - ownerReceivesFix
       - `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_
       - Indicate whether a holding owner of this instrument receives the fix or the default protection leg of the swap\.
     * - fixRate
       - `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_
       - The interest rate of the fix leg\. For example, in case of \"2\.5% fix\" this should be 0\.025\.
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

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-claim-i-57743>` **for** `Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-claims-v4-dynamic-instrument-i-98466>` **for** `Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-base-v4-instrument-i-70415>` **for** `Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-instrument-swap-v0-creditdefault-instrument-i-43747>` **for** `Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085_>`_

  + **interface instance** :ref:`I <type-daml-finance-interface-util-v3-disclosure-i-28317>` **for** `Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085_>`_

Data Types
----------

.. _type-daml-finance-instrument-swap-v0-creditdefault-instrument-t-68763:

**type** `T <type-daml-finance-instrument-swap-v0-creditdefault-instrument-t-68763_>`_
  \= `Instrument <type-daml-finance-instrument-swap-v0-creditdefault-instrument-instrument-63085_>`_

  Type synonym for ``Instrument``\.
