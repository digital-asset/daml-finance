.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Generic Instrument
##################

The :doc:`Generic Instrument<../packages/implementations/daml-finance-instrument-generic>`
provides a flexible framework to model and lifecycle custom payoffs in Daml Finance.
It encapsulates the :doc:`Contingent Claims <generic/contingent-claims>` library, which gives us
the tools to model the economic terms of the payoff.

To follow the code snippets used in this page in Daml Studio, you can
`clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_ and run the
scripts in the `Instrument/Generic/Test/Intermediated/BondCoupon.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml>`_
and `Instrument/Generic/Test/EuropeanOption.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml>`_
files.

The Generic Instrument and the Contingent Claims library are introduced in the
:doc:`Payoff Modeling tutorial <../tutorials/payoff-modeling/intro>`, which we encourage you to
check out.

How to create a Generic Instrument
**********************************

Define the Claim of a Bond
==========================

Consider a fixed rate bond which pays a 4% coupon *per annum* with a coupon period of 6 months.
Assume that there are two coupons remaining until maturity: one to be paid today and one to be paid
in 180 days. This can be modeled in the following way:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- CREATE_CC_INSTRUMENT_VARIABLES_BEGIN
  :end-before: -- CREATE_CC_INSTRUMENT_VARIABLES_END

Now that we have specified the economic terms of the payoff we can create a generic instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/Intermediated/BondCoupon.daml
  :language: daml
  :start-after: -- CREATE_CC_INSTRUMENT_BEGIN
  :end-before: -- CREATE_CC_INSTRUMENT_END

On every coupon payment date, the issuer will need to lifecycle the instrument. This will result in
a lifecycle effect for the coupon, which can be then be claimed and settled. This process is
described in detail in
:doc:`Getting Started: Lifecycling <../tutorials/getting-started/lifecycling>`.

Define the Claim of a European Option
=====================================

Alternatively, if you want to model a European Option instead, you can define the claim as follows

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- CREATE_CC_OPTION_INSTRUMENT_VARIABLES_BEGIN
  :end-before: -- CREATE_CC_OPTION_INSTRUMENT_VARIABLES_END

This uses the :ref:`european <function-contingentclaims-core-v3-builders-european-38509>` builder
function, which is included in :doc:`Contingent Claims <generic/contingent-claims>`.

Compared to the bond, where the passage of time results in a coupon payment being due, the
option instrument requires a manual *Election*: the holder of the instrument holding needs to choose
whether or not to exercise the option. How this is done is described in the next section.

How to lifecycle a Generic Instrument
*************************************

.. _election-based-lifecycling:

Election based lifecycling of Contingent Claims based instruments
=================================================================

We describe how to lifecycle an option instrument (which can be *Exercised* or *Expired*), but the
same concepts apply to other Election based instruments (for example, a callable bond that can be
*Called* or *NotCalled*). A similar workflow is also applicable to some of the instruments
available in the Bond, Swap, and Option packages.

First, an Election factory is created:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- CREATE_ELECTION_FACTORY_BEGIN
  :end-before: -- CREATE_ELECTION_FACTORY_END

Then, election offers are created for the different election choices that are available.
Specifically, for option instruments, an election offer to *exercise* is created:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- CREATE_ELECTION_OFFER_EXERCISE_BEGIN
  :end-before: -- CREATE_ELECTION_OFFER_EXERCISE_END

Similarly, an election offer to *expire* the option is also created:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- CREATE_ELECTION_OFFER_EXPIRE_BEGIN
  :end-before: -- CREATE_ELECTION_OFFER_EXPIRE_END

Assuming the investor wants to exercise the option, an election candidate contract is created. In
order to do this, the investor presents a holding for which an election should be made, and also
specifies the amount that this election applies to. This amount cannot exceed the quantity of the
holding:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- CREATE_TOO_BIG_ELECTION_CANDIDATE_BEGIN
  :end-before: -- CREATE_TOO_BIG_ELECTION_CANDIDATE_END

Instead, the elected amount must be the same as the holding quantity, or lower:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- CREATE_ELECTION_CANDIDATE_BEGIN
  :end-before: -- CREATE_ELECTION_CANDIDATE_END

A time event is also required to indicate *when* the election is made:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- CREATE_CLOCK_BEGIN
  :end-before: -- CREATE_CLOCK_END

It is now possible to create the *Election*:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- CREATE_ELECTION_BEGIN
  :end-before: -- CREATE_ELECTION_END

Note: these templates (election offer and election candidate) are not considered a core part of the
Daml Finance library. There can be different processes to create the Election, so this is rather
application specific. Still, in order to showcase one way how this could be done, this workflow is
included here for convenience.

The :ref:`Election <module-daml-finance-interface-lifecycle-v4-election-15483>`
has a flag *electorIsOwner*, which indicates whether the election is on behalf of the owner of the
holding. This is typically the case for options, where the option holder has the right, but not the
obligation, to exercise the option. On the other hand, for callable bonds it is not the holding
owner (the bond holder) who gets to decide whether the bond is redeemed early. Instead, it is the
counterparty. In this case, *electorIsOwner* would be false.

A lifecycle rule is required to specify how to process the Election:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- CREATE_LIFECYCLE_RULE_BEGIN
  :end-before: -- CREATE_LIFECYCLE_RULE_END

This is similar to time-based lifecycling.

Finally, it is possible to apply the Election according to the lifecycle rule provided:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Generic/Test/EuropeanOption.daml
  :language: daml
  :start-after: -- APPLY_ELECTION_BEGIN
  :end-before: -- APPLY_ELECTION_END

This creates lifecycle effects, which can be claimed and settled in the usual way (as described in
:doc:`Getting Started: Lifecycling <../tutorials/getting-started/lifecycling>`). However, the
holding contract used to claim the effect must be compatible with the election that has been made:
if Alice made an election and *electorIsOwner = True*, then only a holding where *owner = alice*
will be accepted.

Frequently Asked Questions
**************************

When is a Generic Instrument preferred over a bond or swap?
===========================================================

The previously described instruments (e.g. :doc:`Bonds <bond>` and :doc:`Swaps <swap>`) have the
advantage that the instrument terms are reflected as template variables. Also, the name of the
template directly indicates what type of instrument it is.

The Generic instrument has the advantage that it provides a flexible framework for defining new
payoffs in an ad-hoc manner. This can be useful for prototyping or in a structuring context. It also
enables one-off trades for which no other instrument is available.
