.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-structuredproduct-v0-util-44475:

Daml.Finance.Instrument.StructuredProduct.V0.Util
=================================================

Data Types
----------

.. _type-daml-finance-instrument-structuredproduct-v0-util-c-88007:

**type** `C <type-daml-finance-instrument-structuredproduct-v0-util-c-88007_>`_
  \= :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>`

.. _type-daml-finance-instrument-structuredproduct-v0-util-o-5387:

**type** `O <type-daml-finance-instrument-structuredproduct-v0-util-o-5387_>`_
  \= :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>`

Functions
---------

.. _function-daml-finance-instrument-structuredproduct-v0-util-includes-48834:

`includes <function-daml-finance-instrument-structuredproduct-v0-util-includes-48834_>`_
  \: :ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>` \-\> :ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>` \-\> \[`Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_\]

  Find out which schedule periods of scheduleA exist in scheduleB\.

.. _function-daml-finance-instrument-structuredproduct-v0-util-createautocallableclaims-13527:

`createAutoCallableClaims <function-daml-finance-instrument-structuredproduct-v0-util-createautocallableclaims-13527_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> :ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>` \-\> :ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>` \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149>` \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Calculate the claims for an auto\-callable with a contingent coupon on each payment date
  and a redemption amount at maturity (unless auto\-called previously)\.
