.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Getting started : Lifecycling
###############################

This tutorial describes the :ref:`lifecycle <lifecycling>` flow between two counterparties.
We will use a bond instrument to illustrate the different steps:

#. Creating a fixed-rate bond instrument
#. Defining the clock for time-based events
#. Lifecycling the bond instrument
#. Settling the instructions

Download the code for the tutorial
**********************************

The code of this tutorial resides in the `Daml Finance <https://github.com/digital-asset/daml-finance>`_ repo.
You can install it locally by following :doc:`these instructions <install-daml-finance>`.

In particular, the file ``src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml`` is the starting point
of this tutorial.
It also refers to some utility functions in ``src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml``.

Creating a fixed-rate bond instrument
*************************************

We start by defining a fixed rate bond, which pays a 1.1% coupon every year.

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_VARIABLES_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_VARIABLES_END

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_END

We also credit the account of an investor:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml
  :language: daml
  :start-after: -- CREDIT_ACCOUNT_FIXED_RATE_BOND_BEGIN
  :end-before: -- CREDIT_ACCOUNT_FIXED_RATE_BOND_END


Defining the clock for time-based events
****************************************

Since the bond pays a coupon on a yearly basis, we talk about a time-based event.
The requirement to pay the coupon is governed by actual time.
However, in a trading and settlement system, it is useful to be able to control
the time variable, in order to simulate previous/future payments, or to have some flexibility
regarding when to process events.

We define a clock contract to control the passage of time:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_CLOCK_FOR_BOND_LIFECYCLING_BEGIN
  :end-before: -- CREATE_CLOCK_FOR_BOND_LIFECYCLING_END


Lifecycling the bond instrument
*******************************

We use the ``Lifecyclable`` interface, which is defined in ``Daml.Finance.Interface.Lifecycle.Lifecyclable``.

The issuer of the bond is responsible for initiating the coupon payment,
by calling ``Lifecycle`` on the coupon date:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_BEGIN
  :end-before: -- LIFECYCLE_BOND_END

This internally uses the ``Event`` interface, which is defined in ``Daml.Finance.Interface.Lifecycle.Event``. In our case, the event
is a clock event, since the coupon is defined by the passage of time.

The ``effectCids`` will contain the effect(s) of the lifecycling, in this case a coupon payment.
If there is nothing to lifecycle, for example because there is no coupon to be paid today, ``effectCids`` would be empty.
The ``Effect`` interface is defined in ``Daml.Finance.Interface.Lifecycle.Effect``.



Settling the instructions
*************************

In order to process the effect(s) of the lifecycling (in this case: pay the coupon), we need to create settlement instructions.
We start by creating a settlement factory:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_SETTLEMENT_FACTORY_BOND_BEGIN
  :end-before: -- CREATE_SETTLEMENT_FACTORY_BOND_END

The investor then claims the effect:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- CLAIM_EFFECT_BOND_BEGIN
  :end-before: -- CLAIM_EFFECT_BOND_END

Finally, the settlement instructions are allocated, approved and then settled.

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml
  :language: daml
  :start-after: -- ALLOCATE_APPROVE_SETTLE_INSTRUCTIONS_BOND_BEGIN
  :end-before: -- ALLOCATE_APPROVE_SETTLE_INSTRUCTIONS_BOND_END

This is the result of the settlement:
  - The investor receives cash for the coupon.
  - The investor receives a new version of the bond instrument, which excludes today's coupon (it only contains future coupons and the redemption amount).
  - The issuer receives the original version of the bond instrument, which can be archived.

