.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Tutorial : How to use the Bond extension package
################################################

Download the code for the tutorial
**********************************

The code of this tutorial resides in the `Daml Finance <https://github.com/digital-asset/daml-finance>`_ repo.
You can install it locally by following :doc:`these instructions <../getting-started/install-daml-finance>`.

In particular, the Bond test folder ``src/test/daml/Daml/Finance/Instrument/Bond/Test/`` is the starting point
of this tutorial.

How to use the Bond extension in your app
*****************************************

As explained in the :ref:`Getting Started <structure-of-code-dependencies>` section
and on the :doc:`Architecture <../architecture>` page,
your app should only depend on the interface layer of Daml Finance.
For bonds this means that you should only include the bond interface package:
``Daml.Finance.Interface.Instrument.Bond``.

Your initialization scripts are an exception to this, since they are only run once when your app is initialized.
These would create the factories needed. Your app can then create bonds through these factory interfaces.

How to create a bond instrument
*******************************

There are different types of bonds, which mainly differ in the way the coupon is defined.
In order to create a bond instrument you first have to decide what type it is.
The bond extension package currently supports the following bond types:

Fixed rate
==========

Fixed rate bonds pay a constant coupon each coupon period. The coupon is quoted on a yearly basis (per annum, p.a.), but it could be paid more frequently.
For example, a bond could have a 2% p.a. coupon and a 6M coupon period. That would mean
a 1% coupon is paid twice a year.

As an example we will create a bond instrument paying a 1.1% p.a. coupon with a 12M coupon period.
This example is taken from ``src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml``,
where all the details are available.

We start by defining the terms:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_VARIABLES_END

The :ref:`day count convention <type-daml-finance-interface-types-date-daycount-daycountconventionenum-67281>` is used to determine how many days (i.e. what fraction of a full year)
each coupon period has. This will determine the exact coupon amount each period.

The :ref:`business day convention <type-daml-finance-interface-types-date-calendar-businessdayconventionenum-88986>` determines how a coupon date is adjusted if it
falls on a non-business day.

Now that we have defined the terms we can create the bond instrument:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_END

Once the instrument is created, you can book a holding on it using ``Account.credit``.

Floating rate
=============

Floating rate bonds pay a coupon which is determined by a reference rate.
There is also a rate spread, which is paid in addition to the reference rate.

Here is an example of a bond paying Euribor 3M + 1.1% p.a. with a 3M coupon period:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/FloatingRate.daml
  :language: daml
  :start-after: -- CREATE_FLOATING_RATE_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_FLOATING_RATE_BOND_VARIABLES_END

Here is how we create the floating rate bond instrument:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_FLOATING_RATE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FLOATING_RATE_BOND_INSTRUMENT_END

The reference rate is observed once at the beginning of each coupon period and used for
the coupon payment at the end of that period.

Inflation linked
================

Inflation linked bonds pay a fixed coupon rate at the end of every coupon period.
The coupon is calculated based on a principal that is adjusted according to an inflation index,
for example the Consumer Price Index (CPI) in the U.S.

Here is an example of a bond paying 1.1% p.a. (on a CPI adjusted principal) with a 3M coupon period:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/InflationLinked.daml
  :language: daml
  :start-after: -- CREATE_INFLATION_LINKED_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_INFLATION_LINKED_BOND_VARIABLES_END

Then, we create the inflation linked bond instrument:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_INFLATION_LINKED_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_INFLATION_LINKED_BOND_INSTRUMENT_END

At maturity, the greater of the adjusted principal and the original principal is redeemed.
For clarity, this only applies to the redemption amount. The coupons are always calculated based on the adjusted principal.
This means that in the case of deflation, the coupons would be lower than the specified coupon rate but the original principal would still be redeemed at maturity.


Zero coupon
===========

A zero coupon bond does not pay any coupons at all.
It only pays the redemption amount at maturity.

Here is an example of a zero coupon bond:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/ZeroCoupon.daml
  :language: daml
  :start-after: -- CREATE_ZERO_COUPON_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_ZERO_COUPON_BOND_VARIABLES_END

Finally, we create the zero coupon bond instrument:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_ZERO_COUPON_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_ZERO_COUPON_BOND_INSTRUMENT_END



How to trade and transfer a bond
********************************

When you have created a holding on a bond instrument this can be transfered to another party.
This is described in :doc:`Getting Started: Transfer <../getting-started/transfer>`.

In order to trade a bond (transfer it in exchange for cash) you can also initiate a delivery versus payment with atomic settlement.
This is described in :doc:`Getting Started: Settlement <../getting-started/settlement>`.

How to process coupon payments
******************************

On the coupon payment date, the issuer will need to lifecycle the bond.
This will result in a lifecycle effect for the coupon, which can be cash settled.
This is described in detail in :doc:`Getting Started: Lifecycling <../getting-started/lifecycling>`.

How to redeem a bond
********************

On the redemption date, both the last coupon and the redemption amount with be paid.
This is processed in the same way as a single coupon payment described above.
