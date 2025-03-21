.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Bond Instrument
###############

To follow the code snippets used in this page in Daml Studio, you can
`clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_ and run the
scripts included in the
`Instrument/Bond/Test <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Bond/Test>`_
folder.

How to use a Bond Instrument in your application
************************************************

As explained in the :ref:`Getting Started <structure-of-code-dependencies>` section and on the
:doc:`Architecture <../overview/architecture>` page, your app should only depend on the interface
layer of Daml Finance. For bonds this means that you should only include the
:doc:`bond interface package <../packages/interfaces/daml-finance-interface-instrument-bond>`.

Your initialization scripts are an exception, since they are only run once when your app is
initialized. These are used to create the necessary instrument factories. Your app can then create
bond instruments through these factory contracts.

How to Create a Bond Instrument
*******************************

There are different types of bonds, which mainly differ in the way the coupon is defined. In order
to create a bond instrument you first have to decide what type of bond you need. The
:doc:`bond instrument package <../packages/implementations/daml-finance-instrument-bond>`
currently supports the following bond types:

.. _fixed-rate-bond-tutorial-section:

Fixed Rate
==========

:ref:`Fixed rate bonds <module-daml-finance-instrument-bond-v3-fixedrate-instrument-89221>`
pay a constant coupon rate at the end of each coupon period. The coupon is quoted on a yearly basis
(per annum, p.a.), but it could be paid more frequently (e.g. quarterly). For example, a bond could
have a 2% p.a. coupon and a 6M coupon period. That would mean a 1% coupon is paid twice a year.

As an example we will create a bond instrument paying a 1.1% p.a. coupon with a 12M coupon period.
This example is taken from
`Instrument/Bond/Test/FixedRate.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml>`_
, where all the details are available.

We start by defining the terms:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_VARIABLES_END

The :ref:`day count convention <type-daml-finance-interface-types-date-v3-daycount-daycountconventionenum-31>`
is used to determine how many days, i.e., what fraction of a full year, each coupon period has. This
will determine the exact coupon amount that will be paid each period.

The :ref:`business day convention <type-daml-finance-interface-types-date-v3-calendar-businessdayconventionenum-14112>`
determines *how* a coupon date is adjusted if it falls on a non-business day.

We also need holiday calendars, which determine *when* to adjust dates.

We can use these variables to create a
:ref:`PeriodicSchedule <constr-daml-finance-interface-types-date-v3-schedule-periodicschedule-24577>`:

.. literalinclude:: ../src/test/daml/Daml/Finance/Test/Util/Time.daml
  :language: daml
  :start-after: -- CREATE_PERIODIC_SCHEDULE_BEGIN
  :end-before: -- CREATE_PERIODIC_SCHEDULE_END

This is used to determine the periods that are used to calculate the coupon. There are a few things
to note here:

- The :ref:`RollConventionEnum <type-daml-finance-interface-types-date-v3-rollconvention-rollconventionenum-89490>`
  defines whether dates are rolled at the end of the month or on a given date of the month. In our
  example above, we went for the latter option.
- The :ref:`StubPeriodTypeEnum <type-daml-finance-interface-types-date-v3-schedule-stubperiodtypeenum-99734>`
  allows you to explicitly specify what kind of stub period the bond should have. This is optional
  and not used in the example above. Instead, we defined the stub implicitly by specifying a
  ``firstRegularPeriodStartDate``: since the time between the issue date and the first regular
  period start date is less than 12M (our regular coupon period), this implies a short initial
  stub period.

For more information on calendar, schedule, and day count functions, see the
:doc:`date utility functions tutorial <../tutorials/advanced-topics/instrument-modeling/date-utilities>`.

Now that we have defined the terms we can create the bond instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_END

Once this is done, you can create a holding on it using
:ref:`Account.Credit <module-daml-finance-interface-account-v4-account-30007>`.

Floating Rate
=============

:ref:`Floating rate bonds <module-daml-finance-instrument-bond-v3-floatingrate-instrument-82370>`
pay a coupon which is determined by a reference rate.
There is also a rate spread, which is paid in addition to the reference rate.

Here is an example of a bond paying Euribor 3M + 1.1% p.a. with a 3M coupon period:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/FloatingRate.daml
  :language: daml
  :start-after: -- CREATE_FLOATING_RATE_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_FLOATING_RATE_BOND_VARIABLES_END

The instrument supports two types of reference rates, which are configurable using the
:ref:`ReferenceRateTypeEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-referenceratetypeenum-15522>`:

- Libor/Euribor style rates with a single fixing
- SOFR style reference rates (using a compounded index)

Using these terms we can create the floating rate bond instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_FLOATING_RATE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FLOATING_RATE_BOND_INSTRUMENT_END

The reference rate (Euribor 3M) is observed once at the beginning of each coupon period and used
for the coupon payment at the end of that period.

Callable
========

:ref:`Callable bonds <module-daml-finance-instrument-bond-v3-callable-instrument-35206>` are similar to
the bonds above, but in addition they can be redeemed by the issuer before maturity. The callability
is restricted to some (or all) of the coupon dates. In other words, these bonds have a *Bermudan*
style embedded call option.

Both fixed and floating rate coupons are supported by this instrument.
In case of a floating rate, there is often a fixed spread as well. This can be represented by
a fixed rate coupon, which is shown in the following example. Here is a bond paying
Libor 3M + 0.1% p.a. with a 3M coupon period:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/Callable.daml
  :language: daml
  :start-after: -- CREATE_3M_CAP_FLOOR_FLOATING_6M_CALLABLE_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_3M_CAP_FLOOR_FLOATING_6M_CALLABLE_BOND_VARIABLES_END

The coupon rate in this example also has a 0% floor and a 1.5% cap. This is configurable, just set
the cap or floor to *None* if it does not apply.

The fixed rate is fairly simple to define, but the floating rate requires more inputs. A
:ref:`FloatingRate <type-daml-finance-interface-instrument-types-v2-floatingrate-floatingrate-56149>`
data type is used to specify which reference rate should be used and on which date the reference
rate is fixed for each coupon period.

The above variables can be used to create a *couponSchedule*:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/Callable.daml
  :language: daml
  :start-after: -- CREATE_3M_CAP_FLOOR_FLOATING_6M_CALLABLE_BOND_COUPON_SCHEDULE_BEGIN
  :end-before: -- CREATE_3M_CAP_FLOOR_FLOATING_6M_CALLABLE_BOND_COUPON_SCHEDULE_END

This *couponSchedule* is used to determine the coupon payment dates, where the
*businessDayConvention* specifies how dates are adjusted. Also, *useAdjustedDatesForDcf* determines
whether adjusted or unadjusted dates should be used for day count fractions (to determine the coupon
amount).

In addition to the Libor/Euribor style reference rates, compounded SOFR and similar reference rates
are also supported. In order to optimize performance, these compounded rates are calculated via a
(pre-computed) continuously compounded index, as described in the
:ref:`ReferenceRateTypeEnum <type-daml-finance-interface-instrument-types-v2-floatingrate-referenceratetypeenum-15522>`.
For example, here is how *daily compounded SOFR* can be specified using the *SOFR Index*:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/Callable.daml
  :language: daml
  :start-after: -- CREATE_6M_SOFR_CALLABLE_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_6M_SOFR_CALLABLE_BOND_VARIABLES_END

This instrument also allows you to configure on which coupon dates the bond is callable. This is
done by specifying a separate *callSchedule*. The bond is callable on the *last* date of each
schedule period. For example, if the bond is callable on every coupon date, simply set
*callSchedule = couponSchedule*. Alternatively, if the bond is only callable every six months, this
can be configured by specifying a different schedule:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/Callable.daml
  :language: daml
  :start-after: -- CREATE_3M_CAP_FLOOR_FLOATING_6M_CALLABLE_BOND_SCHEDULE_BEGIN
  :end-before: -- CREATE_3M_CAP_FLOOR_FLOATING_6M_CALLABLE_BOND_SCHEDULE_END

The *noticeDays* field defines how many business days notice is required to call the bond.
The election whether or not to call the bond must be done on this date.

Using these terms we can create the callable bond instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_MULTI_SCHEDULE_CALLABLE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_MULTI_SCHEDULE_CALLABLE_BOND_INSTRUMENT_END

Unlike regular fixed and floating bonds, which are lifecycled based on the passage of time, this
callable bond instrument contains an embedded option that is not automatically exercised. Instead,
the custodian of the bond holding must manually decide whether or not to call the bond. This is done
by making an *Election*.

This callable bond example is taken from
`Instrument/Bond/Test/Callable.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Bond/Test/Callable.daml>`_
, where all the details are available. Also, check out the
:ref:`Election based lifecycling tutorial <election-based-lifecycling>` for more details on how to
define and process an *Election* in practice. Note that the sample bond above, which is callable
only on some of the coupon dates, will require two types of lifecycling:

- :doc:`Time based lifecycling <../tutorials/getting-started/lifecycling>` on coupon dates when the
  bond is *not* callable.
- :ref:`Election based lifecycling <election-based-lifecycling>` on coupon dates when the bond *is*
  callable.

Inflation-Linked
================

There are different types of inflation-linked bonds in the marketplace. The
:ref:`Inflation-linked bonds <module-daml-finance-instrument-bond-v3-inflationlinked-instrument-99606>`
currently supported in Daml Finance pay a fixed coupon rate at the end of every coupon period. This
corresponds to the payoff of e.g. Treasury Inflation-Protected Securities (TIPS) that are issued by
the U.S. Treasury. The coupon is calculated based on a principal that is adjusted according to an
inflation index, for example the Consumer Price Index (CPI) in the U.S.

Here is an example of a bond paying 1.1% p.a. (on a CPI adjusted principal) with a 3M coupon period:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/InflationLinked.daml
  :language: daml
  :start-after: -- CREATE_INFLATION_LINKED_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_INFLATION_LINKED_BOND_VARIABLES_END

Based on these terms we can create the inflation linked bond instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_INFLATION_LINKED_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_INFLATION_LINKED_BOND_INSTRUMENT_END

At maturity, the greater of the adjusted principal and the original principal is redeemed. For
clarity, this only applies to the redemption amount. The coupons are always calculated based on the
adjusted principal. This means that in the case of deflation, the coupons would be lower than the
specified coupon rate but the original principal would still be redeemed at maturity.

Zero Coupon
===========

A :ref:`zero coupon bond <module-daml-finance-instrument-bond-v3-zerocoupon-instrument-96672>`
does not pay any coupons at all. It only pays the redemption amount at maturity.

Here is an example of a zero coupon bond:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/ZeroCoupon.daml
  :language: daml
  :start-after: -- CREATE_ZERO_COUPON_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_ZERO_COUPON_BOND_VARIABLES_END

Based on this we create the zero coupon bond instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_ZERO_COUPON_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_ZERO_COUPON_BOND_INSTRUMENT_END

Frequently Asked Questions
**************************

How do I transfer or trade a bond?
==================================

When you have created a holding on a bond instrument this can be transferred to another party.
This is described in the :doc:`Getting Started: Transfer <../tutorials/getting-started/transfer>`
tutorial.

In order to trade a bond (transfer it in exchange for cash) you can also initiate a delivery versus
payment with atomic settlement. This is described in the
:doc:`Getting Started: Settlement <../tutorials/getting-started/settlement>` tutorial.

How do I process coupon payments for a bond?
============================================

On the coupon payment date, the issuer will need to lifecycle the bond. This will result in a
lifecycle effect for the coupon, which can be cash settled. This is described in detail in the
:doc:`Lifecycling <../tutorials/getting-started/lifecycling>` and the
:doc:`Intermediated Lifecycling <../tutorials/advanced-topics/lifecycling/intermediated-lifecycling>`
tutorials.

How do I redeem a bond?
=======================

On the redemption date, both the last coupon and the redemption amount will be paid. This is
processed in the same way as a single coupon payment described above.

How do I view the terms of a bond instrument?
=============================================

There are several ways to access the data of a contract, as explained in the
:ref:`Patterns <getview>` section.
