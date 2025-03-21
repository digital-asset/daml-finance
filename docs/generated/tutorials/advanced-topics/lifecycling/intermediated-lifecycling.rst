.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Intermediated Lifecycling of an Instrument
##########################################

This tutorial describes the :ref:`lifecycling <lifecycling>` flow of an instrument with an
intermediary party between the issuer and the investor. We will use the a
:doc:`Generic Instrument <../../../instruments/generic>`, but the same concepts apply to other
instrument types as well.

We will illustrate the following steps:

#. Creating a Generic Instrument modeling a fixed rate bond
#. Defining an intermediated settlement route
#. Defining a suitable lifecycle event
#. Lifecycling the instrument
#. Non-atomic settlement of the lifecycle effects
#. Atomic settlement of the lifecycle effects

To follow the script used in this tutorial, you can
`clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_. In particular,
the file
`Instrument/Generic/Test/Intermediated/BondCoupon.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml>`_
is the starting point of this tutorial. It contains an example for both non-atomic and atomic
settlement of lifecycle effects. In this tutorial we will focus on the non-atomic settlement, but we
will mention atomic settlement towards the end.

Create the Instrument
*********************

We start by using a :doc:`Generic Instrument <../../../instruments/generic>` to model a fixed rate
bond paying a 4% p.a. coupon with a 6M coupon period.

Define an Intermediated Settlement Route
****************************************

In the case of intermediated lifecycling, we need to define a settlement route for the bond
instrument, which depends on the account structure:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- CREATE_BOND_ROUTE_BEGIN
  :end-before: -- CREATE_BOND_ROUTE_END

Similarly, we define a settlement route for the cash instrument instrument:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- CREATE_CASH_ROUTE_BEGIN
  :end-before: -- CREATE_CASH_ROUTE_END

Define a Lifecycle Event
************************

Since the bond pays a coupon twice a year, payment is a time-based event. The requirement to
pay the coupon is governed by actual time. However, in a trading and settlement system, it is useful
to be able to control the time variable, in order to simulate previous/future payments, or to have
some flexibility regarding when to process events.

Because of this, the issuer defines a clock update event contract, which signals that a certain time
has been reached:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- CREATE_CLOCK_FOR_BOND_LIFECYCLING_BEGIN
  :end-before: -- CREATE_CLOCK_FOR_BOND_LIFECYCLING_END

Lifecycle the Bond Instrument
*****************************

Using the :ref:`Lifecycle <module-daml-finance-interface-lifecycle-v4-rule-lifecycle-8270>` interface,
the CSD creates a lifecycle rule contract:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_CREATE_RULE_BEGIN
  :end-before: -- LIFECYCLE_BOND_CREATE_RULE_END

The issuer of the bond is responsible for initiating the lifecycling of the coupon payment, by
exercising the ``Evolve`` choice on the coupon date:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_BEGIN
  :end-before: -- LIFECYCLE_BOND_END

This internally uses the :ref:`Event <module-daml-finance-interface-lifecycle-v4-event-91777>`
interface. In our case, the event is a clock update event, since the coupon payment is triggered by
the passage of time.

The return type of ``effectCid`` is an
:ref:`Effect <module-daml-finance-interface-lifecycle-v4-effect-48507>` interface.
It will contain the effect(s) of the lifecycling, in this case a coupon payment. If
there is nothing to lifecycle, for example because there is no coupon to be paid today,
this would be empty.

Non-atomic Settlement
*********************

In order to process the effect(s) of the lifecycling (in this case: pay the coupon), we need to
create settlement instructions. In the non-atomic case, this is done in two steps.

First, there is the settlement between the issuer and the CSD. By using the
``EffectSettlementService`` template, the issuer can claim and settle the lifecycling effects in one
step by exercising the ``ClaimAndSettle`` choice:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_ISSUER_CSD_BEGIN
  :end-before: -- LIFECYCLE_BOND_ISSUER_CSD_END

Then, there is the settlement between the CSD and the investor. We start by creating a settlement
factory:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_SETTLEMENT_FACTORY_BEGIN
  :end-before: -- LIFECYCLE_BOND_SETTLEMENT_FACTORY_END

Settlement instructions are created
by using the :ref:`Claim <module-daml-finance-interface-lifecycle-v4-rule-claim-89954>` interface and
exercising the ``ClaimEffect`` choice:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_CSD_INVESTOR_BEGIN
  :end-before: -- LIFECYCLE_BOND_CSD_INVESTOR_END

Claiming the effect has two consequences:

- the investor's holding is upgraded to a new instrument version (where the coupon has been paid)
- settlement instructions are generated in order to process the coupon payment

Finally, the settlement instructions are allocated, approved and then settled.

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_ALLOCATE_APPROVE_SETTLE_BEGIN
  :end-before: -- LIFECYCLE_BOND_ALLOCATE_APPROVE_SETTLE_END

Following settlement, the investor receives a cash holding for the due coupon amount.

Atomic Settlement
*****************

In the non-atomic settlement case above, settlement was done in two steps: first from issuer to CSD
and then from CSD to investor. In atomic settlement, this is done in on step.

The first part of the process is very similar. The first important difference is when the CSD
exercises the ``ClaimEffect`` choice, where the bond holdings of both the CSD and the investor are
provided:

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_ATOMIC_CLAIMEFFECT_BEGIN
  :end-before: -- LIFECYCLE_BOND_ATOMIC_CLAIMEFFECT_END

There are now more settlement instructions (both from CSD to issuer and from issuer to CSD):

.. literalinclude:: ../../../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_ATOMIC_INSTRUCTIONS_BEGIN
  :end-before: -- LIFECYCLE_BOND_ATOMIC_INSTRUCTIONS_END

These will have to be allocated, approved and settled similarly to the non-atomic case above.
See the file
`Instrument/Generic/Test/Intermediated/BondCoupon.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml>`_
for full details.

Frequently Asked Questions
**************************

What if one party wants to cancel the settlement?
=================================================

The parties who sign the
:ref:`Batch <module-daml-finance-interface-settlement-v4-batch-88127>` contract (the instructor
and consenters) can exercise the ``Cancel`` choice of the
:ref:`Batch <module-daml-finance-interface-settlement-v4-batch-88127>` to cancel all associated
:ref:`Instructions <module-daml-finance-interface-settlement-v4-instruction-71097>`
atomically.

Settlement can also be obstructed if the responsible parties do not fully allocate
and approve the settlement instructions.
