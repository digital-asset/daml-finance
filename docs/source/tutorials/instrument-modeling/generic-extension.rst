.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

How To Use the Generic Extension To Model Generic Instruments
################################################################

To follow the script used in this tutorial you can `clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_.
In particular, the file ``src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml`` is the starting point
of this tutorial.

How To Create a Generic Instrument
**********************************

The ``Generic`` extension provides a flexible framework to model generic instruments in Daml Finance.
It encapsulates the ``Contingent Claims`` library, which allows us to model the economic terms of an instrument.

Define the Claim
================

Consider a fixed rate bond which pays a 4% p.a. coupon with a 6M coupon period.
Assume there are two coupons remaining until maturity: one today and one in 180 days.
This could be modeled in the following way using ``Contingent Claims``:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- CREATE_CC_INSTRUMENT_VARIABLES_BEGIN
  :end-before: -- CREATE_CC_INSTRUMENT_VARIABLES_END

Now that we have specified the economic terms we can create a generic instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- CREATE_CC_INSTRUMENT_BEGIN
  :end-before: -- CREATE_CC_INSTRUMENT_END

This will create an instrument containing the ``Contingent Claims`` tree on the ledger.

How To Trade and Transfer a Generic Instrument
**********************************************

When you have created a holding on the instrument above it can be transfered to another party.
This is described in :doc:`Getting Started: Transfer <../getting-started/transfer>`.

In order to trade the instrument (transfer it in exchange for cash) you can also initiate a delivery versus payment with atomic settlement.
This is described in :doc:`Getting Started: Settlement <../getting-started/settlement>`.

How to Process Lifecycle Events
*******************************

On a coupon payment date of the bond instrument above, the issuer will need to lifecycle the instrument.
This will result in a lifecycle effect for the coupon, which can be cash settled.
This is described in detail in :doc:`Getting Started: Lifecycling <../getting-started/lifecycling>`.

How to Redeem a Generic Instrument
**********************************

On the redemption date, both the last coupon and the redemption amount with be paid.
This is processed in the same way as a single coupon payment described above.
