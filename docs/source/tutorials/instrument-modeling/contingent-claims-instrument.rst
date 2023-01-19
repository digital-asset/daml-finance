.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

How to Implement a Contingent Claims-Based Instrument
#####################################################

In this chapter we will look at how to create a strongly typed instrument, which leverages the
:doc:`Contingent Claims <../../concepts/contingent-claims>` library. As an example, we will see how
the
:ref:`fixed rate bond instrument <module-daml-finance-instrument-bond-fixedrate-instrument-67993>`
is implemented in Daml Finance. The goal is that you will learn how to implement your own instrument
template, if you need an instrument type that is not already implemented in Daml Fincance.

To follow the script used in this tutorial, you can
`clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_. In particular,
the file ``src/test/daml/Daml/Finance/Instrument/Bond/Test/FixedRate.daml`` is the starting point of
this tutorial. It also refers to some utility functions in
``src/test/daml/Daml/Finance/Instrument/Bond/Test/Util.daml``.

Template Definition
===================

We start by defining a new template for the instrument. Here are the first few lines of the fixed
rate instrument:

.. literalinclude:: ../../../../src/main/daml/Daml/Finance/Instrument/Bond/FixedRate/Instrument.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_TEMPLATE_BEGIN
  :end-before: -- FIXED_RATE_BOND_TEMPLATE_END

These template variables describe the payoff of the fixed rate bond. They will be used to create a
:doc:`Contingent Claims <../../concepts/contingent-claims>` tree, which is the internal
representation used for modelling and lifecycling in Daml Finance. Note that this tree is not part
of the template above. Instead, it will be created dynamically, as described in the next sections.

The Claims Interface
====================

In order for the instrument to work with the general Daml Finance lifecycling framework, we will
implement the :ref:`Claims interface <module-daml-finance-interface-claims-claim-82866>`.
This provides a generic mechanism to process coupon payments and the redemption amount.
It will work in a similar way for all instrument types, regardless of their economic terms.

Here is a high level implementation of the
:ref:`Claims interface <module-daml-finance-interface-claims-claim-82866>`:

.. literalinclude:: ../../../../src/main/daml/Daml/Finance/Instrument/Bond/FixedRate/Instrument.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_CLAIMS_BEGIN
  :end-before: -- FIXED_RATE_BOND_CLAIMS_END

The ``getClaims`` function is where we define the payoff of the instrument.
First, we create a coupon schedule, which depends on the coupon dates and a holiday calendar. This
is then used to create the actual coupon claims. The redemption claim is also created. Finally, the
coupon claims and the redemption claim are joined. Together, they define the economic terms of the
instrument.

How to Define the Redemption Claim
==================================

In the above example, we see that the redemption claim depends on the currency and the maturity
date.

We will now create the actual redemption claim:

.. literalinclude:: ../../../../src/main/daml/Daml/Finance/Claims/Util/Builders.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_REDEMPTION_CLAIM_BEGIN
  :end-before: -- FIXED_RATE_BOND_REDEMPTION_CLAIM_END

Keywords like
:ref:`when <function-contingentclaims-core-claim-when-17123>`,
:ref:`TimeGte <constr-contingentclaims-core-internal-claim-timegte-91610>`,
:ref:`scale <function-contingentclaims-core-claim-scale-79608>`,
:ref:`one <function-contingentclaims-core-claim-one-13168>` and
:ref:`give <function-contingentclaims-core-claim-give-6964>`
are defined in the :doc:`Contingent Claims documentation <../../concepts/contingent-claims>`.

How to Define the Coupon Claims
===============================

The coupon claims are a bit more complicated to define.
We need to take a schedule of adjusted coupon dates and the day count convention into account.

Here is how we create the coupon claims:

.. literalinclude:: ../../../../src/main/daml/Daml/Finance/Claims/Util/Builders.daml
  :language: daml
  :start-after: -- FIXED_RATE_BOND_COUPON_CLAIMS_BEGIN
  :end-before: -- FIXED_RATE_BOND_COUPON_CLAIMS_END

For each coupon period, we calculate the adjusted end date and the amount of the coupon. We then
create each coupon claim in a way similar to the redemption claim above.

How the Instrument Evolves Over Time
====================================

The bond instrument gives the holder the right to receive future coupons and the redemption amount.
At issuance, this means all the coupons, since they are all in the future. However, when the first
coupon is paid, the holder of the instrument is no longer entitled to receive this coupon again.
In other words, the claims representation of the instrument changes. It evolves over time.

In our implementation of the fixed rate bond, we want a simple and reliable mechanism for evolving
the instrument. Luckily for us, when the lifecycle function returns a coupon to be paid today, it
also returns the remaining claims of the instrument (excluding today's and any previous coupons).
Hence, we can use this to evolve our instrument, in a way that is guaranteed to be consistent with
the lifecycle mechanism.

This is all done in the :ref:`Lifecycle.Rule <module-daml-finance-claims-lifecycle-rule-53980>`. We will now break it apart to describe the
steps in more detail:

.. literalinclude:: ../../../../src/main/daml/Daml/Finance/Claims/Lifecycle/Rule.daml
  :language: daml
  :start-after: -- BOND_PROCESS_CLOCK_UPDATE_INITAL_CLAIMS_BEGIN
  :end-before: -- BOND_PROCESS_CLOCK_UPDATE_INITAL_CLAIMS_END

First, we retrieve the inital claims of the instrument. This represents the bond as of inception.
By keeping track of ``lastEventTimestamp`` (in our case: the last time a coupon was paid), we can
"fast forward" to the remaining claims of the instrument:

.. literalinclude:: ../../../../src/main/daml/Daml/Finance/Claims/Lifecycle/Rule.daml
  :language: daml
  :start-after: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_FASTFORWARD_BEGIN
  :end-before: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_FASTFORWARD_END

Finally, we can lifecycle the instrument as of the current time.
If there is a lifecycle effect (for example a coupon), we will create an
:ref:`Effect <module-daml-finance-lifecycle-effect-1975>`
for it, which can then be settled.

.. literalinclude:: ../../../../src/main/daml/Daml/Finance/Claims/Lifecycle/Rule.daml
  :language: daml
  :start-after: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_BEGIN
  :end-before: -- BOND_PROCESS_CLOCK_UPDATE_LIFECYCLE_END

Observables
===========

In our fixed rate bond example above, the coupon amount is pre-determined at the inception of the
instrument. In contrast, a floating rate coupon is defined by the value of a reference rate during
the lifetime of the bond. Since we do not know this value when the instrument is created, we need to
define the coupon based on a future observation of the reference rate. In order to do this we
introduce the concept of a
:ref:`numeric observation <module-daml-finance-data-numeric-observation-78761>`.

In the instrument definition, we need an identifier for the reference rate:

.. literalinclude:: ../../../../src/main/daml/Daml/Finance/Instrument/Bond/FloatingRate/Instrument.daml
  :language: daml
  :start-after: -- FLOATING_RATE_BOND_TEMPLATE_UNTIL_REFRATE_BEGIN
  :end-before: -- FLOATING_RATE_BOND_TEMPLATE_UNTIL_REFRATE_END

When we create the claims, we can then use
:ref:`Observe <constr-contingentclaims-core-observation-observe-30391>`
to refer to the value of the reference rate:

.. literalinclude:: ../../../../src/main/daml/Daml/Finance/Claims/Util/Builders.daml
  :language: daml
  :start-after: -- FLOATING_RATE_BOND_COUPON_CLAIMS_BEGIN
  :end-before: -- FLOATING_RATE_BOND_COUPON_CLAIMS_END

In this example, the observable is an interest reference rate. Other instrument types can require
other types of observables, for example an FX rate or a stock price.
