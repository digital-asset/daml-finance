.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Election-based lifecycling (using a callable bond)
##################################################

This tutorial describes how to define and process Elections. It builds on the previous
:doc:`Time-based Lifecycling <fixed-rate-bond>` tutorial, which uses a fixed rate bond where all
coupons are pre-defined and are paid out as time passes. In contrast, the coupons of a callable bond
depend on whether the issuer has called the bond. Hence, a simple time event is not sufficient to
define the next state of the instrument. Instead, the lifecycling framework requires an active
Election to be made on each call date. This Election is the main topic of the tutorial.
Check out the :ref:`Lifecycling concepts <time-vs-election-lifecycling>` for more details on time
based vs election based evolution of instruments.

In this tutorial, we are going to:

#. create a callable bond instrument and book a holding on it
#. reuse the lifecycle rule and settlement factory from the fixed rate bond tutorial
#. create the election not to call the bond
#. process the election event to produce the effects of a coupon payment
#. instruct settlement by presenting a bond holding
#. settle the resulting batch atomically

Run the Script
**************

The code for this tutorial can be executed via the ``runCallableBond`` script in the
``CallableBond.daml`` module.

Instrument and Holding
======================

In order to demonstrate the
:ref:`Election <module-daml-finance-interface-lifecycle-v4-election-15483>` concept, we need a suitable
sample instrument.
:ref:`Callable bonds <module-daml-finance-instrument-bond-v3-callable-instrument-35206>` pay coupons as
long as the bond has not been called by the issuer.
The :doc:`Bond Instrument packages <../../instruments/bond>` page describes this
instrument in more detail. Here, we briefly show how to create the bond instrument using a factory:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/CallableBond.daml
  :language: daml
  :start-after: -- CREATE_CALLABLE_BOND_INSTRUMENT_BEGIN
  :end-before: -- CREATE_CALLABLE_BOND_INSTRUMENT_END

Compared to the fixed rate bond, notice that this callable instrument also has a ``callSchedule``,
that specifies the dates on which the issuer can call the bond.

We also create a bond holding in Bob's account:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/CallableBond.daml
  :language: daml
  :start-after: -- CREATE_CALLABLE_BOND_HOLDING_BEGIN
  :end-before: -- CREATE_CALLABLE_BOND_HOLDING_END

Now, we have both an instrument definition and a holding. Let us proceed to lifecycle the bond using
``Elections``, which is the main purpose of this tutorial.

Lifecycle Events and Rule
=========================

We start by creating an Election factory, which can be used to create elections:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/CallableBond.daml
  :language: daml
  :start-after: -- CREATE_ELECTION_FACTORY_BEGIN
  :end-before: -- CREATE_ELECTION_FACTORY_END

An :ref:`Election <module-daml-finance-interface-lifecycle-v4-election-15483>` contains three main
pieces of information:

- the election tag (e.g. "CALLED")
- who is making the election (e.g. the bank)
- the date to which it applies.

In our example, the bank chooses *not* to call the bond:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/CallableBond.daml
  :language: daml
  :start-after: -- CREATE_ELECTION_BEGIN
  :end-before: -- CREATE_ELECTION_END

Note the flag *electorIsOwner* above. Since the bank is not the owner of the bond holding, this flag
is *False* in our example. On the other hand, if an investor Alice would have had a holding in a
puttable bond, the election whether or not to put would have belonged to Alice (the holding owner),
so this flag would have been *True*.

Also, note that there is an *amount* in the election above. This allows the elector to create an
election for a specific number of holding units.

Now, we have what we need to actually lifecycle the bond. The ``Apply`` choice is exercised in order
to process the election:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/CallableBond.daml
  :language: daml
  :start-after: -- LIFECYCLE_BOND_BEGIN
  :end-before: -- LIFECYCLE_BOND_END

In order to lifecycle the coupon payment above, we need a lifecycle rule that defines how to process
all election events. The lifecycle rule from the previous tutorial can be reused for this, if we
first convert it to an ``Election.Exercisable``, as described above.

A :ref:`Claim Rule <module-daml-finance-lifecycle-v4-rule-claim-11721>` allows the elector to claim the
effect resulting from the election event:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/CallableBond.daml
  :language: daml
  :start-after: -- CREATE_CLAIM_RULE_BEGIN
  :end-before: -- CREATE_CLAIM_RULE_END

Note that even though we already had a claim rule in the previous example, we could not reuse it
because that one was for the *holding owner* to claim the results, whereas in the case of Election
based lifecycling it is the *elector* that should claim them:

.. literalinclude:: ../../finance-lifecycling/daml/Scripts/CallableBond.daml
  :language: daml
  :start-after: -- CLAIM_EFFECT_BEGIN
  :end-before: -- CLAIM_EFFECT_END

The result of this is an effect describing the per-unit asset movements to be executed for bond
holders.

The remaining steps (settling the entitlements) are identical to the previous tutorial.

Note that the election process above is not limited to callable bonds. It also works for other
instruments that require a manual decision, such as a physically settled option with a manual
exercise decision.

Frequently Asked Questions
**************************

Which party should create the elections?
========================================

This depends on the economics of the instrument. For example, in a callable bond, it is the issuer
of the bond that has the right to choose whether or not to call the bond on the call dates. On the
other hand, in the case of a puttable bond, it would be the investor that can elect to demand early
repayment of the bond.

What if a bond can only be called on some coupon dates?
=======================================================

Some instruments can require both time based and election based lifecycling. For example, consider
a callable bond that has a quarterly coupon but a call schedule that only allows the bond to be
called once a year. In this case, an Election has to be created on the call dates to lifecycle the
bond. On the other coupon dates, regular time based lifecycling is required to process the coupon
payments.

Summary
*******

You have learned how to create a callable bond and how to define Elections to choose whether or not
to call the bond.
The key concepts to take away are:

* Elections are required in order to lifecycle some instruments that require an *active choice* by
  one of the stakeholders.
* Depending on the economics of the instrument, either the holding owner or the issuer should create
  the election.
* Some instruments require both time based and election based lifecycling.
