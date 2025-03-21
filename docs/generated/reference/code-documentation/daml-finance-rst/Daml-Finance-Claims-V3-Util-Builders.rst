.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-claims-v3-util-builders-30825:

Daml.Finance.Claims.V3.Util.Builders
====================================

This module includes utility functions used to build contingent claim trees that represent
specific payoffs\. A ``Schedule`` is usually used as an input to these utility functions\. Given
that schedules are defined in terms of dates, a claim where the time parameter is ``Date``
is returned\. These are then mapped to claims where the time parameter is ``Time`` using a
(user\-provided) conversion function\.

Data Types
----------

.. _type-daml-finance-claims-v3-util-builders-c-48597:

**type** `C <type-daml-finance-claims-v3-util-builders-c-48597_>`_
  \= :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>`

.. _type-daml-finance-claims-v3-util-builders-fixingdates-31766:

**type** `FixingDates <type-daml-finance-claims-v3-util-builders-fixingdates-31766_>`_
  \= :ref:`DateOffset <type-daml-finance-interface-types-date-v3-dateoffset-dateoffset-75159>`

.. _type-daml-finance-claims-v3-util-builders-o-737:

**type** `O <type-daml-finance-claims-v3-util-builders-o-737_>`_
  \= :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>`

Functions
---------

.. _function-daml-finance-claims-v3-util-builders-prepareandtagclaims-21771:

`prepareAndTagClaims <function-daml-finance-claims-v3-util-builders-prepareandtagclaims-21771_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> \[`C <type-daml-finance-claims-v3-util-builders-c-48597_>`_\] \-\> `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Convert the claims to UTCTime and tag them\.

.. _function-daml-finance-claims-v3-util-builders-createfixratepaymentclaimslist-63253:

`createFixRatePaymentClaimsList <function-daml-finance-claims-v3-util-builders-createfixratepaymentclaimslist-63253_>`_
  \: :ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>` \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> \[`C <type-daml-finance-claims-v3-util-builders-c-48597_>`_\]

.. _function-daml-finance-claims-v3-util-builders-createfixratepaymentclaims-27509:

`createFixRatePaymentClaims <function-daml-finance-claims-v3-util-builders-createfixratepaymentclaims-27509_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> :ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>` \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Calculate a fix rate amount for each payment date and create claims\.

.. _function-daml-finance-claims-v3-util-builders-createratepaymentclaims-74999:

`createRatePaymentClaims <function-daml-finance-claims-v3-util-builders-createratepaymentclaims-74999_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> :ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>` \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149>` \-\> :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Calculate a floating rate amount for each payment date and create claims\.
  This is a general function that supports both LIBOR and SOFR\-COMPOUND reference rates\.
  It also supports a fix rate spread\. If no floating rate is provided, only the fix spread is used,
  i\.e\. fix rate claims are created\.

.. _function-daml-finance-claims-v3-util-builders-createconditionalcreditfixratepaymentclaims-48891:

`createConditionalCreditFixRatePaymentClaims <function-daml-finance-claims-v3-util-builders-createconditionalcreditfixratepaymentclaims-48891_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> :ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>` \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>` \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Calculate a fix rate amount (if a credit event has not yet happened) for each payment date and
  create claims\.

.. _function-daml-finance-claims-v3-util-builders-createcrediteventpaymentclaims-78091:

`createCreditEventPaymentClaims <function-daml-finance-claims-v3-util-builders-createcrediteventpaymentclaims-78091_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>` \-\> :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Calculate a (1\-recoveryRate) payment if a credit event just happened and create claims\.

.. _function-daml-finance-claims-v3-util-builders-createassetperformancepaymentclaims-53211:

`createAssetPerformancePaymentClaims <function-daml-finance-claims-v3-util-builders-createassetperformancepaymentclaims-53211_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> :ref:`Schedule <type-daml-finance-interface-types-date-v3-schedule-schedule-18327>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>` \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> \[(`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_)\] \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Calculate the asset performance for each payment date and create claims\.
  The performance is calculated using the weighted performance of the basket underlyings from the
  start date to the end date of each payment period\.
  The reference asset Observables need to contain the appropriate type of fixings\:

  * unadjusted fixings in case of a price return asset swap
  * adjusted fixings in case of a total return asset swap

.. _function-daml-finance-claims-v3-util-builders-createfxadjustedprincipalclaim-27271:

`createFxAdjustedPrincipalClaim <function-daml-finance-claims-v3-util-builders-createfxadjustedprincipalclaim-27271_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Create an FX adjusted principal claim\.
  This can be used for both FX swaps (using the appropriate FX rate) and single currency bonds
  (setting the FX rate to 1\.0)\.

.. _function-daml-finance-claims-v3-util-builders-createvanillaoptionclaim-23305:

`createVanillaOptionClaim <function-daml-finance-claims-v3-util-builders-createvanillaoptionclaim-23305_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>` \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `C <type-daml-finance-claims-v3-util-builders-c-48597_>`_

  Create the claim for a long vanilla option (cash\-settled, automatically exercised)\.

.. _function-daml-finance-claims-v3-util-builders-createeuropeancashclaim-21554:

`createEuropeanCashClaim <function-daml-finance-claims-v3-util-builders-createeuropeancashclaim-21554_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>` \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Create the claim for a cash\-settled, automatically exercised option (long or short)\.

.. _function-daml-finance-claims-v3-util-builders-createbarriereuropeancashclaim-79552:

`createBarrierEuropeanCashClaim <function-daml-finance-claims-v3-util-builders-createbarriereuropeancashclaim-79552_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>` \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Create the claim for a barrier option (automatically exercised, cash\-settled)\.

.. _function-daml-finance-claims-v3-util-builders-createeuropeanphysicalclaim-64168:

`createEuropeanPhysicalClaim <function-daml-finance-claims-v3-util-builders-createeuropeanphysicalclaim-64168_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Create the claim for a physically settled European option\.

.. _function-daml-finance-claims-v3-util-builders-createdividendoptionclaim-35992:

`createDividendOptionClaim <function-daml-finance-claims-v3-util-builders-createdividendoptionclaim-35992_>`_
  \: (`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_) \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>` \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>` \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`InstrumentQuantity <type-daml-finance-interface-types-common-v3-types-instrumentquantity-36264>` \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Create the claim for a physically settled Dividend option\.
