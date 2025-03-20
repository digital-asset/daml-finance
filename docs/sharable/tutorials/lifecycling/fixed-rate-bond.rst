.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Time-based lifecycling (using a fixed rate bond)
################################################

This tutorial describes how to lifecycle instruments with pre-defined payments, e.g. a fixed rate
bond. It is similar to the :doc:`Lifecycling tutorial <../getting-started/lifecycling>`, in that it
describes how lifecycle rules and events can be used to evolve instruments over time. However, there
is one main difference:

* The :doc:`Lifecycling tutorial <../getting-started/lifecycling>` describes a dividend event, which
  is something that the issuer defines *on an ongoing basis*. Only once the date and amount of a
  dividend payment has been defined, the issuer creates a distribution event accordingly.
* This tutorial describes a fixed rate bond, where all coupon payments are defined *in advance*.
  They are all encoded in the instrument definition. Hence, the issuer does not need to create
  distribution events on an ongoing bases. Instead, one lifecycle rule in combination with time
  events (a date clock) are used to lifecycle the bond.

Check out the :doc:`Lifecycling concepts <../../concepts/lifecycling>` for a more in-depth
description of the evolution of financial instruments over their lifetime.

In this tutorial, we are going to:

#. create a fixed rate bond instrument and book a holding on it
#. create a lifecycle rule
#. create a lifecycle event (time event: ``DateClockUpdate``)
#. process the event to produce the effects of a coupon payment
#. instruct settlement by presenting a bond holding
#. settle the resulting batch atomically

This example builds on the previous :doc:`Settlement <../getting-started/settlement>` tutorial
script. Some required concepts are explained there, so please check out that tutorial before you
continue below.

Run the Script
**************

The code for this tutorial can be executed via the ``runFixedRateBond`` script in the
``FixedRateBond.daml`` module.

Instrument and Holding
======================

For the purpose of showcasing time-based lifecycling, we need a suitable sample instrument.
:ref:`Fixed rate bonds <module-daml-finance-instrument-bond-v3-fixedrate-instrument-89221>`
pay a constant coupon rate at the end of each coupon period. The
:doc:`Bond Instrument packages <../../instruments/bond>` page describes this instrument
in more detail. Here, we briefly show how to create the bond instrument using a factory:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FixedRateBond.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_INSTRUMENT_END

We also create a bond holding in Bob's account:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FixedRateBond.daml
  :language: daml
  :start-after: -- CREATE_FIXED_RATE_BOND_HOLDING_BEGIN
  :end-before: -- CREATE_FIXED_RATE_BOND_HOLDING_END

A holding represents the ownership of a certain amount of an :ref:`instrument <instrument>` by an
owner at a custodian. Check out the :doc:`Holdings <../getting-started/holdings>` tutorial for more
details.

Now, we have both an instrument definition and a holding. Let us proceed to lifecycle the bond,
which is the main purpose of this tutorial.

Lifecycle Events and Rule
=========================

As mentioned earlier, we only need one single lifecycle rule to process all time events (since all
coupon payments are pre-defined in the instrument terms):

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FixedRateBond.daml
  :language: daml
  :start-after: -- CREATE_LIFECYCLE_RULE_BEGIN
  :end-before: -- CREATE_LIFECYCLE_RULE_END

In order to lifecycle a coupon payment, we create a time event corresponding to the date of the
first coupon:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FixedRateBond.daml
  :language: daml
  :start-after: -- CREATE_CLOCK_UPDATE_EVENT_BEGIN
  :end-before: -- CREATE_CLOCK_UPDATE_EVENT_END

Note that it is the bank that actively creates a
:ref:`DateClockUpdateEvent <module-daml-finance-data-v4-time-dateclockupdate-59678>`.
This results in more control when to actually process the coupon payment. One could also use
:ref:`LedgerTime <module-daml-finance-data-v4-time-ledgertime-80144>`,
but that could cause problems in some scenarios, for example:

- The system is down when the coupon should be processed. Processing it the next day is difficult if
  ledger time is automatically used, because the date returned from the ledger no longer matches the
  intended lifecycling date.
- A global ledger containing trades from regions in different time zones may require flexibility
  regarding when, in relation to ledger time, to process coupons and other events.
- The coupon payment depends on market data, which the data provider occasionally provides with a
  delay. Retroactively processing this is simpler if the lifecycler can provide the *today* date.

Now, we have what we need to actually lifecycle the bond. The ``Evolve`` choice of the lifecycle
rule is exercised to process the time event:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FixedRateBond.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_BEGIN
  :end-before: -- LIFECYCLE_BOND_END

Both the :ref:`LedgerTime <module-daml-finance-data-v4-time-ledgertime-80144>` and the
:ref:`DateClock <module-daml-finance-data-v4-time-dateclock-1389>` implement the
:ref:`TimeObservable <module-daml-finance-interface-lifecycle-v4-observable-timeobservable-64296>`
interface, which is used by ``Evolve`` to specify the current time for the lifecycling.

The result of this is an effect describing the per-unit asset movements to be executed for bond
holders. Each holder can now present their holding to *claim* the effect and instruct settlement of
the associated entitlements.

A :ref:`Claim Rule <module-daml-finance-lifecycle-v4-rule-claim-11721>` allows a holder of the
target instrument to claim the effect resulting from the time event:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FixedRateBond.daml
  :language: daml
  :start-after: -- CREATE_CLAIM_RULE_BEGIN
  :end-before: -- CREATE_CLAIM_RULE_END

By presenting their holding they can instruct the settlement of the holding transfers described in
the effect:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FixedRateBond.daml
  :language: daml
  :start-after: -- CLAIM_EFFECT_BEGIN
  :end-before: -- CLAIM_EFFECT_END

As a side-effect of settling the entitlements, the presented holding is exchanged for a holding of
a new bond version. This is to prevent a holder from benefiting from a given effect twice (in our
case: receiving the same coupon twice).

In our example of a bond coupon, only a single instruction is generated: the movement of cash from
the bank to the bond holder. This instruction along with its batch is settled the usual way, as
described in the previous :doc:`Settlement <../getting-started/settlement>` tutorial.

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FixedRateBond.daml
  :language: daml
  :start-after: -- EFFECT_SETTLEMENT_BEGIN
  :end-before: -- EFFECT_SETTLEMENT_END

Note that the bank in this case does not actually transfer the cash from another account, but simply
credits Bob's account by using the ``CreditReceiver`` allocation type. In a real-world bond coupon
scenario one would additionally model the flow of funds from the issuer to the bank using the same
lifecycle process as described above.

Check out the :doc:`Settlement concepts <../../concepts/settlement>` for a more in-depth
description of the different steps in the settlement process and the settlement modes supported
by the library.

Note that the lifecycling process above is not limited to fixed coupon bonds. It also works for
other instruments with pre-defined payments, for example:

+-------------------------+-------------------------------------+
| Instrument              | Pre-defined variable                |
+=========================+=====================================+
| Foreign exchange swaps  | FX rate                             |
+-------------------------+-------------------------------------+
| Currency swaps          | Interest rates (on both legs)       |
+-------------------------+-------------------------------------+

Frequently Asked Questions
**************************

Which party should create and sign the lifecycle rules and events?
==================================================================

In the simplified scenario for this tutorial, we have used the bank as both the *issuer* and
*depository* for the instruments involved. In a real-world case, instruments and their corresponding
lifecycle rules and events would be maintained by an actual issuer, with the depository acting as a
3rd-party trust anchor.

Which parties typically take which actions in the lifecycle workflow?
=====================================================================

The lifecycle interfaces governing the process leave the controllers of the various choices in the
process up to the implementation.

* Typically, we would expect the issuer of an instrument to define the instrument terms, which in
  our example above govern the date and amount of each bond coupon.
* Lifecycle rules on the other hand are often controlled by 3rd-party calculation agents.
* The claiming of lifecycle effects is by default the responsibility of the owner of a holding. If
  instead the owner wants to delegate this responsibility to their custodian they can do so via a
  delegation contract.
* The party executing settlement can be chosen as well, as described in the previous tutorial on
  :doc:`Settlement <../getting-started/settlement>`.

Summary
*******

You have learned how to create a fixed coupon bond and how to use a lifecycle rule and events to
process the payments pre-defined by the instrument.
The key concepts to take away are:

* Lifecycle events cause the bond instrument to evolve over time.
* A lifecycle rule contains logic to calculate the effects an event has on an instrument and its
  holdings.
* A claim rule is used to instruct settlement for a given effect using a holding.
