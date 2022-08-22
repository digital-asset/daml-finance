.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

.. _module-daml-finance-instrument-bond-util-36904:

Module Daml.Finance.Instrument.Bond.Util
=============================

Functions
---------

.. _function-daml-finance-instrument-bond-util-createcouponschedule-28327:

`createCouponSchedule <function-daml-finance-instrument-bond-util-createcouponschedule-28327_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> \[`Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_\] \-\> :ref:`BusinessDayConventionEnum <type-daml-finance-common-date-calendar-businessdayconventionenum-67582>` \-\> :ref:`PeriodEnum <type-daml-finance-common-date-rollconvention-periodenum-40915>` \-\> `Int <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-int-37261>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ :ref:`Schedule <type-daml-finance-common-date-schedule-schedule-53649>`

  Retrieve holiday calendar(s) from the ledger and create a coupon schedule

.. _function-daml-finance-instrument-bond-util-prepareandtagclaims-41864:

`prepareAndTagClaims <function-daml-finance-instrument-bond-util-prepareandtagclaims-41864_>`_
  \: `Applicative <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-prelude-applicative-9257>`_ f \=\> \[Claim `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ :ref:`Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-67765>` :ref:`Observable <type-daml-finance-interface-instrument-generic-types-observable-46520>`\] \-\> `Text <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-text-51952>`_ \-\> f \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758>`\]

  Convert the claims to UTCTime and tag them

.. _function-daml-finance-instrument-bond-util-createfixratecouponclaims-86167:

`createFixRateCouponClaims <function-daml-finance-instrument-bond-util-createfixratecouponclaims-86167_>`_
  \: (`HasField <https://docs.daml.com/daml/stdlib/DA-Record.html#class-da-internal-record-hasfield-52839>`_ \"adjustedEndDate\" r `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_, `HasField <https://docs.daml.com/daml/stdlib/DA-Record.html#class-da-internal-record-hasfield-52839>`_ \"adjustedStartDate\" r `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_, `Applicative <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-prelude-applicative-9257>`_ f) \=\> \[r\] \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`DayCountConventionEnum <type-daml-finance-common-date-daycount-daycountconventionenum-57741>` \-\> :ref:`Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-67765>` \-\> f \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758>`\]

  Calculate a fix coupon amount for each coupon date and create claims

.. _function-daml-finance-instrument-bond-util-createfloatingratecouponclaims-46549:

`createFloatingRateCouponClaims <function-daml-finance-instrument-bond-util-createfloatingratecouponclaims-46549_>`_
  \: (`HasField <https://docs.daml.com/daml/stdlib/DA-Record.html#class-da-internal-record-hasfield-52839>`_ \"adjustedEndDate\" r `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_, `HasField <https://docs.daml.com/daml/stdlib/DA-Record.html#class-da-internal-record-hasfield-52839>`_ \"adjustedStartDate\" r `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_, `Applicative <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-prelude-applicative-9257>`_ f) \=\> \[r\] \-\> `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ \-\> :ref:`DayCountConventionEnum <type-daml-finance-common-date-daycount-daycountconventionenum-57741>` \-\> :ref:`Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-67765>` \-\> :ref:`Observable <type-daml-finance-interface-instrument-generic-types-observable-46520>` \-\> f \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758>`\]

  Calculate a floating coupon amount for each coupon date and create claims

.. _function-daml-finance-instrument-bond-util-createredemptionclaim-94296:

`createRedemptionClaim <function-daml-finance-instrument-bond-util-createredemptionclaim-94296_>`_
  \: `Applicative <https://docs.daml.com/daml/stdlib/Prelude.html#class-da-internal-prelude-applicative-9257>`_ f \=\> :ref:`Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-67765>` \-\> `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> f \[:ref:`TaggedClaim <type-daml-finance-interface-instrument-generic-types-taggedclaim-29758>`\]

  Create a redemption claim

.. _function-daml-finance-instrument-bond-util-datetodateclocktime-74143:

`dateToDateClockTime <function-daml-finance-instrument-bond-util-datetodateclocktime-74143_>`_
  \: `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ \-\> `Time <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-time-63886>`_

  Maps a ``Date`` to ``Time`` using the rule in the ``DateClock``\.
  From the Daml\.Finance\.Gerneric\.Test file, but could not import here (duplicated for now)
  In the termsheet only date is mentioned, but lifecycle logic is based on time\.

.. _function-daml-finance-instrument-bond-util-mapclaimtoutctime-37255:

`mapClaimToUTCTime <function-daml-finance-instrument-bond-util-mapclaimtoutctime-37255_>`_
  \: Claim `Date <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-date-32253>`_ `Decimal <https://docs.daml.com/daml/stdlib/Prelude.html#type-ghc-types-decimal-18135>`_ :ref:`Deliverable <type-daml-finance-interface-instrument-generic-types-deliverable-67765>` :ref:`Observable <type-daml-finance-interface-instrument-generic-types-observable-46520>` \-\> :ref:`C <type-daml-finance-interface-instrument-generic-types-c-63687>`

  Maps a ``Date`` claim to a ``Time`` claim using the rule in the ``DateClock``\.
  From the Daml\.Finance\.Generic\.Test file, but could not import here (duplicated for now)
  In the termsheet only date is mentioned, but lifecycle logic is based on time\.

.. _function-daml-finance-instrument-bond-util-processclockupdate-63380:

`processClockUpdate <function-daml-finance-instrument-bond-util-processclockupdate-63380_>`_
  \: IsBond t \=\> `Party <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-party-57932>`_ \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-event-i-17082>` \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-clock-i-92808>` \-\> `ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>` \-\> t \-\> \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-observable-i-63746>`\] \-\> `Update <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-update-68072>`_ (`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-lifecyclable-i-34924>`, \[`ContractId <https://docs.daml.com/daml/stdlib/Prelude.html#type-da-internal-lf-contractid-95282>`_ :ref:`I <type-daml-finance-interface-lifecycle-effect-i-11106>`\])

  Rule to process a clock update event\.
