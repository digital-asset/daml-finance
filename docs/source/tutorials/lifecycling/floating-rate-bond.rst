.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Observations (using a floating rate bond)
#########################################

This tutorial describes how to define observations. It builds on the previous
:doc:`Time-based Lifecycling <fixed-rate-bond>` tutorial, which uses a fixed rate bond where all
coupons are pre-defined using a constant annualized rate. In contrast, the coupons of a
floating rate bond depend on the value of a reference rate for each coupon period. Hence, the
lifecycling framework requires the future values of the reference rate. This is referred to as
``Observations``, which is the main topic of this tutorial.

In this tutorial, we are going to:

#. create a floating rate bond instrument and book a holding on it
#. create an observation of the floating rate, which is used to define the coupon payment
#. reuse the lifecycle rule and lifecycle event from the fixed rate bond tutorial
#. process the event to produce the effects of a coupon payment
#. instruct settlement by presenting a bond holding
#. settle the resulting batch atomically

Run the Script
**************

The code for this tutorial can be executed via the ``runFloatingRateBond`` script in the
``FloatingRateBond.daml`` module.

Instrument and Holding
======================

For the purpose of showcasing the
:ref:`Observation <module-daml-finance-data-v4-numeric-observation-19522>` concept,
we need a suitable sample instrument.
:ref:`Floating rate bonds <module-daml-finance-instrument-bond-v3-floatingrate-instrument-82370>`
pay a coupon which is determined by a reference rate, e.g. 3M Libor. The
:doc:`Bond Instrument packages <../../instruments/bond>` page describes this instrument
in more detail. Here, we briefly show how to create the bond instrument using a factory:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FloatingRateBond.daml
  :language: daml
  :start-after: -- CREATE_FLOATING_RATE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FLOATING_RATE_BOND_INSTRUMENT_END

Compared to the fixed rate bond, notice that this floating rate instrument also has a
``referenceRateId``, that specifies which ``Observations`` to use in the lifecycling section below.

We also create a bond holding in Bob's account:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FloatingRateBond.daml
  :language: daml
  :start-after: -- CREATE_FLOATING_RATE_BOND_HOLDING_BEGIN
  :end-before: -- CREATE_FLOATING_RATE_BOND_HOLDING_END

Now, we have both an instrument definition and a holding. Let us proceed to lifecycle the bond using
``Observations``, which is the main purpose of this tutorial.

Lifecycle Events and Rule
=========================

An :ref:`Observation <module-daml-finance-data-v4-numeric-observation-19522>` of a reference rate
contains two pieces of information: the interest rate level and the date to which it applies. The
rate level can be positive or negative. In our example, we have a negative interest rate:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FloatingRateBond.daml
  :language: daml
  :start-after: -- CREATE_FLOATING_RATE_OBSERVATIONS_BEGIN
  :end-before: -- CREATE_FLOATING_RATE_OBSERVATIONS_END

In our case, the bank then creates the observation on the ledger.
The :ref:`Observation <module-daml-finance-data-v4-numeric-observation-19522>` implements the
:ref:`NumericObservable <module-daml-finance-interface-lifecycle-v4-observable-numericobservable-50817>`
interface, which is used during lifecycling.

In order to lifecycle a coupon payment, we need a lifecycle rule that defines how to process all
time events. We also need a time event corresponding to the date of the first coupon. Both of these
are the same as in the previous tutorial using a fixed rate bond, so we will reuse them from there.

Now, we have what we need to actually lifecycle the bond:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/FloatingRateBond.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_BEGIN
  :end-before: -- LIFECYCLE_BOND_END

The difference compared to the previous tutorial is that here we also pass in the observables to
the Evolve choice. They are used to evaluate the reference rate on the relevant fixing date of the
coupon payment currently being lifecycled.

The result of this is an effect describing the per-unit asset movements to be executed for bond
holders. Each holder can now present their holding to *claim* the effect and instruct settlement of
the associated entitlements.

The remaining steps (define a claim rule, claim the effect and settle the entitlements) are
identical to the previous tutorial.

Note that the lifecycling process above is not limited to floating rate bonds, which have
a reference rate as observable. It also works for other instruments that depend on an underlying
asset, for example:

+-----------------------+-------------------------------------------------------+
| Instrument            | Observed variable                                     |
+=======================+=======================================================+
| Inflation linked bond | Inflation index                                       |
+-----------------------+-------------------------------------------------------+
| Interest rate swap    | Reference rate (similar to a floating rate bond)      |
+-----------------------+-------------------------------------------------------+
| Asset swap            | Reference asset                                       |
+-----------------------+-------------------------------------------------------+
| Credit default swap   | Default probability & Recovery rate (two observables) |
+-----------------------+-------------------------------------------------------+
| Vanilla option        | Reference asset (often end of day fixing)             |
+-----------------------+-------------------------------------------------------+
| Barrier option        | Reference asset (often intraday observations)         |
+-----------------------+-------------------------------------------------------+

Frequently Asked Questions
**************************

Which party should create the observations?
===========================================

In the simplified scenario for this tutorial, the bank created the observation. In a real-world
case, it would probably be the issuer (or a 3rd-party reference data agent) that creates the
observations.

Summary
*******

You have learned how to create a floating rate bond and how to define observations that define the
amount of the coupon payments.
The key concepts to take away are:

* Observations are required in order to lifecycle some instruments.
* Observations are a general concept that can be used to model different kind of payoffs, using
  various types of underlyings.
* Lifecycling instruments with observations works in a very similar manner compared to those
  without.
