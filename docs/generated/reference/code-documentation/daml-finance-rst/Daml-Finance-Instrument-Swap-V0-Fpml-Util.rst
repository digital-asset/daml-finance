.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-swap-v0-fpml-util-77135:

Daml.Finance.Instrument.Swap.V0.Fpml.Util
=========================================

Data Types
----------

.. _type-daml-finance-instrument-swap-v0-fpml-util-c-60023:

**type** `C <type-daml-finance-instrument-swap-v0-fpml-util-c-60023_>`_
  \= :ref:`Claim <type-contingentclaims-core-v3-internal-claim-claim-83050>` `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>`

.. _type-daml-finance-instrument-swap-v0-fpml-util-o-77403:

**type** `O <type-daml-finance-instrument-swap-v0-fpml-util-o-77403_>`_
  \= :ref:`Observation <type-contingentclaims-core-v3-observation-observation-12406>` `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ :ref:`Observable <type-daml-finance-interface-claims-v4-types-observable-11919>`

Functions
---------

.. _function-daml-finance-instrument-swap-v0-fpml-util-createcalculationperiodicschedule-76747:

`createCalculationPeriodicSchedule <function-daml-finance-instrument-swap-v0-fpml-util-createcalculationperiodicschedule-76747_>`_
  \: :ref:`CalculationPeriodDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-calculationperioddates-23760>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`

  Create a schedule for calculation periods\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-createpaymentperiodicschedule-56262:

`createPaymentPeriodicSchedule <function-daml-finance-instrument-swap-v0-fpml-util-createpaymentperiodicschedule-56262_>`_
  \: :ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>`

  Create a schedule for payment periods\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-getcalendarsandadjust-48793:

`getCalendarsAndAdjust <function-daml-finance-instrument-swap-v0-fpml-util-getcalendarsandadjust-48793_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> :ref:`BusinessDayAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-9111>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Retrieve holiday calendars and adjust a date as specified in a BusinessDayAdjustments FpML
  element

.. _function-daml-finance-instrument-swap-v0-fpml-util-adjustdateaccordingtobusinessdayadjustments-45594:

`adjustDateAccordingToBusinessDayAdjustments <function-daml-finance-instrument-swap-v0-fpml-util-adjustdateaccordingtobusinessdayadjustments-45594_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> :ref:`BusinessDayAdjustments <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-businessdayadjustments-9111>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_

  Adjust a date as specified in a BusinessDayAdjustments FpML element
  (or not at all if NoAdjustment)

.. _function-daml-finance-instrument-swap-v0-fpml-util-applypaymentdaysoffset-98909:

`applyPaymentDaysOffset <function-daml-finance-instrument-swap-v0-fpml-util-applypaymentdaysoffset-98909_>`_
  \: \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> :ref:`PaymentDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-paymentdates-94528>` \-\> \[:ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`\] \-\> \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\]

  Adjust payment schedule according to paymentDaysOffset (if available)\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-getsinglestubrate-19846:

`getSingleStubRate <function-daml-finance-instrument-swap-v0-fpml-util-getsinglestubrate-19846_>`_
  \: :ref:`StubFloatingRate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-62557>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `O <type-daml-finance-instrument-swap-v0-fpml-util-o-77403_>`_

  Define observable part of claim when one specific floating rate is provided for a stub period\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-getinterpolatedstubrate-51087:

`getInterpolatedStubRate <function-daml-finance-instrument-swap-v0-fpml-util-getinterpolatedstubrate-51087_>`_
  \: :ref:`StubFloatingRate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-62557>` \-\> :ref:`StubFloatingRate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-62557>` \-\> :ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>` \-\> :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> :ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `O <type-daml-finance-instrument-swap-v0-fpml-util-o-77403_>`_

  Linearly interpolates two rates within a period, as specified in
  https\://www\.isda\.org/a/aWkgE/Linear\-interpolation\-04022022\.pdf

.. _function-daml-finance-instrument-swap-v0-fpml-util-getstubratefloating-23338:

`getStubRateFloating <function-daml-finance-instrument-swap-v0-fpml-util-getstubratefloating-23338_>`_
  \: \[:ref:`StubFloatingRate <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubfloatingrate-62557>`\] \-\> :ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>` \-\> :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> :ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `O <type-daml-finance-instrument-swap-v0-fpml-util-o-77403_>`_

  Get the floating stub rate to be used for a stub period\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-getstubrate-20396:

`getStubRate <function-daml-finance-instrument-swap-v0-fpml-util-getstubrate-20396_>`_
  \: :ref:`StubCalculationPeriodAmount <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-stubcalculationperiodamount-55158>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>` \-\> :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>` \-\> :ref:`BusinessDayConventionEnum <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `O <type-daml-finance-instrument-swap-v0-fpml-util-o-77403_>`_

  Get the stub rate to be used for a stub period\.
  Currently, three main options from the FpML schema are supported\:

  1. A fix stubRate\.
  2. One or two floating rates for the stub\.
  3. No specific stub rate defined \-\> use the same rate as is used for regular periods\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-alignpaymentschedule-70469:

`alignPaymentSchedule <function-daml-finance-instrument-swap-v0-fpml-util-alignpaymentschedule-70469_>`_
  \: \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\]

  Align the payment schedule with the calculation schedule\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-verifyfxscheduleandgetid-5325:

`verifyFxScheduleAndGetId <function-daml-finance-instrument-swap-v0-fpml-util-verifyfxscheduleandgetid-5325_>`_
  \: \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> :ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> :ref:`FxLinkedNotionalSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fxlinkednotionalschedule-96927>` \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_, `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ \[`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_\])

.. _function-daml-finance-instrument-swap-v0-fpml-util-getfxrateid-28493:

`getFxRateId <function-daml-finance-instrument-swap-v0-fpml-util-getfxrateid-28493_>`_
  \: \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> :ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_, `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_, `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ \[`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_\])

.. _function-daml-finance-instrument-swap-v0-fpml-util-getratefixingsandcalendars-78007:

`getRateFixingsAndCalendars <function-daml-finance-instrument-swap-v0-fpml-util-getratefixingsandcalendars-78007_>`_
  \: :ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>` \-\> :ref:`ResetDates <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-resetdates-28315>` \-\> \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (\[`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_\], :ref:`HolidayCalendarData <type-daml-finance-interface-types-date-v3-calendar-holidaycalendardata-87370>`)

.. _function-daml-finance-instrument-swap-v0-fpml-util-calculatefixpaymentclaimsfromswapstream-23769:

`calculateFixPaymentClaimsFromSwapStream <function-daml-finance-instrument-swap-v0-fpml-util-calculatefixpaymentclaimsfromswapstream-23769_>`_
  \: :ref:`FixedRateSchedule <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-fixedrateschedule-52475>` \-\> :ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ \[`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_\] \-\> \[(`Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_, `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_)\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\]

  Create claims from swapStream that describes a fixed coupon stream\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-calculateprincipalexchangepaymentclaims-76236:

`calculatePrincipalExchangePaymentClaims <function-daml-finance-instrument-swap-v0-fpml-util-calculateprincipalexchangepaymentclaims-76236_>`_
  \: \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> \[(`Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_, `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_)\] \-\> \[`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_\] \-\> :ref:`PrincipalExchanges <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-principalexchanges-43211>` \-\> :ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`

  Create principal exchange claims\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-roundrate-17286:

`roundRate <function-daml-finance-instrument-swap-v0-fpml-util-roundrate-17286_>`_
  \: `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`Rounding <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-rounding-1657>` \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_

  Apply rounding convention to the rate used in a calculation period\.
  Takes a Rounding FpML object as an input\:
  https\://www\.fpml\.org/spec/fpml\-5\-11\-3\-lcwd\-1/html/confirmation/schemaDocumentation/schemas/fpml\-shared\-5\-11\_xsd/complexTypes/FloatingRateCalculation/finalRateRounding\.html

.. _function-daml-finance-instrument-swap-v0-fpml-util-checkrefratecompounding-33788:

`checkRefRateCompounding <function-daml-finance-instrument-swap-v0-fpml-util-checkrefratecompounding-33788_>`_
  \: :ref:`FloatingRateCalculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-floatingratecalculation-25241>` \-\> (`Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_, `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`DayCountConventionEnum <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`)

  Check whether a FloatingRateCalculation uses a reference rate that needs to be compounded\.
  Seems there is no FpML element that specificies this, but that it is implicit in the rate name,
  for example \"USD\-SOFR\-COMPOUND\"
  If it is a compounded reference rate, also return the daycount convention that was used for the
  corresponding reference index, e\.g\. Act360 in the case of the SOFR Index\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-calculatefloatingpaymentclaimsfromswapstream-39727:

`calculateFloatingPaymentClaimsFromSwapStream <function-daml-finance-instrument-swap-v0-fpml-util-calculatefloatingpaymentclaimsfromswapstream-39727_>`_
  \: :ref:`FloatingRateCalculation <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-floatingratecalculation-25241>` \-\> :ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ \[`Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_\] \-\> \[(`Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_, `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_)\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\]

  Create claims from swapStream that describes a floating coupon stream\.

.. _function-daml-finance-instrument-swap-v0-fpml-util-calculateclaimsfromswapstream-39034:

`calculateClaimsFromSwapStream <function-daml-finance-instrument-swap-v0-fpml-util-calculateclaimsfromswapstream-39034_>`_
  \: :ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>` \-\> :ref:`PeriodicSchedule <type-daml-finance-interface-types-date-v3-schedule-periodicschedule-77368>` \-\> \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> \[:ref:`SchedulePeriod <type-daml-finance-interface-types-date-v3-schedule-scheduleperiod-72606>`\] \-\> `Optional <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-prelude-optional-37153>`_ :ref:`SwapStream <type-daml-finance-interface-instrument-swap-v0-fpml-fpmltypes-swapstream-97822>` \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> `Bool <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-bool-66265>`_ \-\> :ref:`Deliverable <type-daml-finance-interface-claims-v4-types-deliverable-51084>` \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ \[:ref:`TaggedClaim <type-daml-finance-interface-claims-v4-types-taggedclaim-85831>`\]

  Create claims from swapStream that describes a fixed or floating coupon stream\.
