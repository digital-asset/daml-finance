.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

How to Use the Bond Extension Package
#####################################

To follow the script used in this tutorial, you can
`clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_. In particular,
the Bond test folder ``src/test/daml/Daml/Finance/Instrument/Bond/Test/`` is the starting point
of this tutorial.

How to Use the Bond Extension in Your Application
*************************************************

As explained in the :ref:`Getting Started <structure-of-code-dependencies>` section and on the
:doc:`Architecture <../../overview/architecture>` page, your app should only depend on the interface
layer of Daml Finance. For bonds this means that you should only include the
:doc:`bond interface package <../../packages/interfaces/daml-finance-interface-instrument-bond>`.

Your initialization scripts are an exception, since they are only run once when your app is
initialized. This creates the necessary factories. Your app can then create bonds through these
factory interfaces.

How to Create a Bond Instrument
*******************************

There are different types of bonds, which mainly differ in the way the coupon is defined. In order
to create a bond instrument you first have to decide what type of bond you need. The
:doc:`bond extension package <../../packages/implementations/daml-finance-instrument-bond>`
currently supports the following bond types:

Fixed Rate
==========

:ref:`Fixed rate bonds <module-daml-finance-instrument-bond-fixedrate-instrument-67993>`
pay a constant coupon each coupon period. The coupon is quoted on a yearly basis
(per annum, p.a.), but it could be paid more frequently. For example, a bond could have a 2% p.a.
coupon and a 6M coupon period. That would mean a 1% coupon is paid twice a year.

As an example we will create a bond instrument paying a 1.1% p.a. coupon with a 12M coupon period.
This example is taken from ``src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml``,
where all the details are available.

We start by defining the terms:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_VARIABLES_END

The :ref:`day count convention <type-daml-finance-interface-types-date-daycount-daycountconventionenum-67281>`
is used to determine how many days, i.e., what fraction of a full year, each coupon period has. This
will determine the exact coupon amount that will be paid each period.

The :ref:`business day convention <type-daml-finance-interface-types-date-calendar-businessdayconventionenum-88986>`
determines how a coupon date is adjusted if it falls on a non-business day.

We can use these variables to create a :ref:`PeriodicSchedule <constr-daml-finance-interface-types-date-schedule-periodicschedule-99705>`:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Test/Util/Time.daml
  :language: daml
  :start-after: -- CREATE_PERIODIC_SCHEDULE_BEGIN
  :end-before: -- CREATE_PERIODIC_SCHEDULE_END

This is used to determine the periods that are used to calculate the coupon. There are a few things
to note here:

- The :ref:`RollConventionEnum <type-daml-finance-interface-types-date-rollconvention-rollconventionenum-73360>`
  defines whether dates are rolled at the end of the month or on a given date of the month. In our
  example above, we went for the latter option.
- The :ref:`StubPeriodTypeEnum <type-daml-finance-interface-types-date-schedule-stubperiodtypeenum-69372>`
  allows you to explicitly specify what kind of stub period the bond should have. This is optional
  and not used in the example above. Instead, we defined the stub implicitly by specifying a
  ``firstRegularPeriodStartDate``: since the time between the issue date and the first regular
  period start date is less than 12M (our regular coupon period), this implies a short initial
  stub period.

Now that we have defined the terms we can create the bond instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_END

Once this is done, you can create a holding on it using
:ref:`Account.credit <module-daml-finance-interface-account-account-92922>`.

Floating Rate
=============

:ref:`Floating rate bonds <module-daml-finance-instrument-bond-floatingrate-instrument-98586>`
pay a coupon which is determined by a reference rate.
There is also a rate spread, which is paid in addition to the reference rate.

Here is an example of a bond paying Euribor 3M + 1.1% p.a. with a 3M coupon period:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/FloatingRate.daml
  :language: daml
  :start-after: -- CREATE_FLOATING_RATE_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_FLOATING_RATE_BOND_VARIABLES_END

Using these terms we can create the floating rate bond instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_FLOATING_RATE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FLOATING_RATE_BOND_INSTRUMENT_END

The reference rate (Euribor 3M) is observed once at the beginning of each coupon period and used
for the coupon payment at the end of that period.

Inflation Linked
================

:ref:`Inflation linked bonds <module-daml-finance-instrument-bond-inflationlinked-instrument-30250>`
pay a fixed coupon rate at the end of every coupon period. The coupon is
calculated based on a principal that is adjusted according to an inflation index, for example the
Consumer Price Index (CPI) in the U.S.

Here is an example of a bond paying 1.1% p.a. (on a CPI adjusted principal) with a 3M coupon period:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/InflationLinked.daml
  :language: daml
  :start-after: -- CREATE_INFLATION_LINKED_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_INFLATION_LINKED_BOND_VARIABLES_END

Based on these terms we can create the inflation linked bond instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_INFLATION_LINKED_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_INFLATION_LINKED_BOND_INSTRUMENT_END

At maturity, the greater of the adjusted principal and the original principal is redeemed. For
clarity, this only applies to the redemption amount. The coupons are always calculated based on the
adjusted principal. This means that in the case of deflation, the coupons would be lower than the
specified coupon rate but the original principal would still be redeemed at maturity.

Zero Coupon
===========

A :ref:`zero coupon bond <module-daml-finance-instrument-bond-zerocoupon-instrument-52804>`
does not pay any coupons at all. It only pays the redemption amount at maturity.

Here is an example of a zero coupon bond:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/ZeroCoupon.daml
  :language: daml
  :start-after: -- CREATE_ZERO_COUPON_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_ZERO_COUPON_BOND_VARIABLES_END

Based on this we create the zero coupon bond instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_ZERO_COUPON_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_ZERO_COUPON_BOND_INSTRUMENT_END

Frequently Asked Questions
**************************

How do I transfer or trade a bond?
==================================

When you have created a holding on a bond instrument this can be transfered to another party.
This is described in the :doc:`Getting Started: Transfer <../getting-started/transfer>` tutorial.

In order to trade a bond (transfer it in exchange for cash) you can also initiate a delivery versus
payment with atomic settlement. This is described in the
:doc:`Getting Started: Settlement <../getting-started/settlement>` tutorial.

How do I process coupon payments for a bond?
============================================

On the coupon payment date, the issuer will need to lifecycle the bond. This will result in a
lifecycle effect for the coupon, which can be cash settled. This is described in detail in the
:doc:`Lifecycling <../getting-started/lifecycling>` and the
:doc:`Intermediated Lifecycling <intermediated-lifecycling>` tutorials.

How do I redeem a bond?
=======================

On the redemption date, both the last coupon and the redemption amount will be paid. This is
processed in the same way as a single coupon payment described above.
