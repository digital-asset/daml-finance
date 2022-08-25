.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Tutorial : How to implement a Contingent Claims-based instrument
################################################################

In this chapter we will look at how to create a strongly typed instrument, which leverages
the ``Contingent Claims`` library.
As an example, we will see how the fixed rate bond instrument is implemented in Daml Finance.
The goal is that you will learn how to implement your own instrument template, if you need an
instrument type that is not already implemented in Daml Fincance.

Download the code for the tutorial
==================================

The code of this tutorial resides in the `Daml Finance <https://github.com/digital-asset/daml-finance>`_ repo.
You can install it locally by following :doc:`these instructions <../getting-started/install-daml-finance>`.

In particular, the file ``src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml`` is the starting point
of this tutorial.
It also refers to some utility functions in ``src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml``.

Template definition
===================

We start by defining a new template for the instrument. Here are the first few lines of the fixed rate instrument:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/FixedRate.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_TEMPLATE_BEGIN
  :end-before: -- FIXED_RATE_BOND_TEMPLATE_END

The ``Contingent Claims`` tree is not part the template. Instead, it will be created
dynamically, as described in the next sections.

HasClaims interface
===================

In order for the instrument to work with the general Daml Finance lifecycling framework
we will implement the HasClaims interface. This provides a generic mechanism to
process coupon payments and the redemption amount. The good thing here is that it will
work in a similar way for all instrument types, regardless of their economic terms.

Here is a high level implementation of HasClaims:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/FixedRate.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_HASCLAIMS_BEGIN
  :end-before: -- FIXED_RATE_BOND_HASCLAIMS_END

First, we create a coupon schedule, which depends on the coupon dates and a holiday calendar.
This is then used to create the actual coupon claims.
The redemption claim is also created.
Finally, the the coupon claims and the redemption claim define the economic terms of the instrument.

How to define the redemption claim
==================================

In the above example, we see that the redemption claim depends on the currency and the maturity date.

We will now create a ``Contingent Claims`` representation of the actual redemption claim:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/Util.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_REDEMPTION_CLAIM_BEGIN
  :end-before: -- FIXED_RATE_BOND_REDEMPTION_CLAIM_END

How to define the coupon claims
===============================

The coupon claims are a bit more complicated to define.
We need to take a schedule of adjusted coupon dates and the day count convention into account.

Here is how we create the ``Contingent Claims`` representation of the coupons:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/Util.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_COUPON_CLAIMS_BEGIN
  :end-before: -- FIXED_RATE_BOND_COUPON_CLAIMS_END

For each coupon period we calculate the adjusted end date and the amount of the coupon.
We then create each coupon claim in a way similar to the redemption claim above.

How the instrument evolves over time
====================================

The bond instrument gives the holder the right to receive future coupons and the redemption amount.
At issuance, this means all the coupons, since they are all in the future.
However, when the first coupon is paid, the holder of the instrument is no longer entitled to receive this coupon again.
In other words, the claims representation of the instrument changes. It evolves over time.

In our implementation of the fixed rate bond we want a simple and reliable mechanism for evolving the instrument.
Luckily for us, when the lifecycle function returns a coupon to be paid today, it also returns the remaining claims of the instrument
(excluding today's and any previous coupons). Hence, we can use this to evolve our instrument, in a way that is guaranteed to be
consistent with the lifecycle mechanism.

This is all done in the ``processClockUpdate`` function. We will now break it apart to describe the steps in more detail:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/Util.daml
  :language: daml
  :start-after: -- BOND_PROCESS_CLOCK_UPDATE_INITAL_CLAIMS_BEGIN
  :end-before: -- BOND_PROCESS_CLOCK_UPDATE_INITAL_CLAIMS_END

First, we retrieve the inital claims of the instrument.
This represents the bond as of inception.
By keeping track of ``lastEventTimestamp`` (in our case: the last time a coupon was paid),
we can "fast forward" to the remaining claims of the instrument:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/Util.daml
  :language: daml
  :start-after: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_FASTFORWARD_BEGIN
  :end-before: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_FASTFORWARD_END

Finally, we can lifecycle the instrument as of the current time (as descibed by the Clock template).
If there is a lifecycle effect (for example a coupon), we will create an Effect for it, which can then be settled.

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/Util.daml
  :language: daml
  :start-after: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_BEGIN
  :end-before: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_END

Observables
===========

In our fixed rate bond example above, the coupon amount is pre-determined at the
inception of the instrument. In contrast, a floating rate coupon is defined by the
value of a reference rate during the lifetime of the bond. Since we do not know this
value when the instrument is created, we need to define the coupon based on a
future observation of the reference rate.
In order to do this we introduce the concept of an ``Observable``.

In the instrument definition, we need an identifier for the reference rate:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/FloatingRate.daml
  :language: daml
  :start-after: -- FLOATING_RATE_BOND_TEMPLATE_UNTIL_REFRATE_BEGIN
  :end-before: -- FLOATING_RATE_BOND_TEMPLATE_UNTIL_REFRATE_END

In the claims definition, we can then use ``Observe`` to refer to the value of the reference rate:

.. literalinclude:: ../../../src/main/daml/Daml/Finance/Instrument/Bond/Util.daml
  :language: daml
  :start-after: -- FLOATING_RATE_BOND_COUPON_CLAIMS_BEGIN
  :end-before: -- FLOATING_RATE_BOND_COUPON_CLAIMS_END

In this example, the observable is an interest reference rate. Other instrument types can require other
types of observables, for example an FX rate or a stock price.
