.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Basic Builders
##############

This tutorial introduces the basic claim constructors and shows how to use them to
describe a payoff in terms of the future cashflows and other `effects <#lifecycling-effect>`__
between the claim's owner and their counterparty.
At the end of this section, you should be able to model payoffs such as fixed rate bonds and
FX forwards.

You can use the ``PayoffBuilder`` module to follow along and test the claims described
below.
When you run the ``runCreateAndLifecycle`` script, it will

- create a Daml Finance Generic instrument wrapping your input claim
- lifecycle the instrument at the specified dates
- print out pending cashflows

Builders
********

Zero
====

The ``zero`` constructor is used to indicate the absence of cashflows and other contractual events.
We can setup this very simple initial payoff as follows

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- ZERO_CLAIM_SETUP_START
  :end-before: -- ZERO_CLAIM_SETUP_END

The acquisition date is used to track the date at which two parties enter the contract and it is
a required input to each claim.

One
===

The ``one`` constructor is used to deliver to the owner of the contract one unit of a specified
instrument. For instance, the claim

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- ONE_CLAIM_SETUP_START
  :end-before: -- ONE_CLAIM_SETUP_END

gives the owner an "immediate" right to receive one unit of the ``USD`` instrument.

We can verify that by lifecycling the claim: we define a set of lifecycle dates

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- ONE_LD_START
  :end-before: -- ONE_LD_END

and run the script to obtain

.. code-block:: none

  "--- EFFECT on 2023-08-01 ---"
  "TARGET INSTRUMENT : MyClaim version 0"
  "RECEIVING"
  " => 1.0 USD"
  "GIVING"

When we lifecycle as of ``01 Aug 2023`` a payment of 1 ``USD`` is received by the owner. This
is recorded in the corresponding ``Effect`` contract.
The claim then becomes worthless (it becomes the ``zero`` claim) and any subsequent
lifecycling yields no additional effects.

Scale
=====

The ``scale`` constructor is used to multiply a claim's `effects <#lifecycling-effect>`__ by a
certain factor.

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- SCALE_CLAIM_SETUP_START
  :end-before: -- SCALE_CLAIM_SETUP_END

As expected, lifecycling now yields

.. code-block:: none

  "--- EFFECT on 2023-08-01 ---"
  "TARGET INSTRUMENT : MyClaim version 0"
  "RECEIVING"
  " => 100.0 USD"
  "GIVING"

Give, And
=========

The ``and`` constructor is used to sum `effects <#lifecycling-effect>`__ from multiple sub-claims.

``give`` is used to exchange rights and obligations, flipping the direction of
`effects <#lifecycling-effect>`__.

We can define a very simple FX trade as follows, where the owner receives ``EUR`` in exchange
for ``USD``.

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- GIVEAND_CLAIM_SETUP_START
  :end-before: -- GIVEAND_CLAIM_SETUP_END

When the claim is lifecycled, we obtain

.. code-block:: none

  "--- EFFECT on 2023-08-01 ---"
  "TARGET INSTRUMENT : MyClaim version 0"
  "RECEIVING"
  " => 90.0 EUR"
  "GIVING"
  " => 100.0 USD"

When you want to additively combine more than two claims, you can use the ``andList`` constructor.

.. warning::

  By default, the ``and`` operator is the :ref:`function <function-da-internal-prelude-and-20777>`
  defined in the Daml standard library. To use the claim constructor instead, use a qualified
  import. Alternatively, you can hide the function from the standard library by using the syntax

  .. code-block:: daml

    import Prelude hiding (and)

  The same applies to the ``or`` operator, which is not covered in this section.

When
====

The ``when`` constructor is used to introduce a time shift, delaying the acquisition of another
claim to a point in the future when a certain predicate is met. For instance, the claim

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- WHEN_CLAIM_SETUP_START
  :end-before: -- WHEN_CLAIM_SETUP_END

pays one ``USD`` once the maturity date is reached, but not before.

When this is lifecycled before maturity, no effect is generated. On the other hand, once we
reach maturity we observe

.. code-block:: none

  "--- EFFECT on 2023-08-31 ---"
  "TARGET INSTRUMENT : MyClaim version 0"
  "RECEIVING"
  " => 1.0 USD"
  "GIVING"

The ``at`` function is used to construct a predicate which becomes ``True`` exactly at the
input date, triggering the acquisition of the sub-claim ``one "USD"``.

Structuring financial instruments
*********************************

Equipped with these basic claim builders, we can already structure a variety of real-world
financial instruments.

Fixed Rate Bond
===============

A fixed rate bond pays a fixed interest rate over its term and repays the principal amount
at maturity. This can be represented as follows

.. code-block:: daml

  interestAmount = Const 50.0
  principal = Const 100000.0
  c = andList [
      when (at d1) $ scale interestAmount $ one "USD"
    , when (at d2) $ scale interestAmount $ one "USD"
    , when (at maturity) $ scale (interestAmount + principal) $ one "USD"
    ]

for ``d1 ≤ d2 ≤ maturity``.

FX Forward
==========

As an exercise, try to model an FX Forward, which is a contractual agreement between two
parties to exchange a pair of currencies at a set rate on a future date.

Summary
*******

You have learned the basic claim constructors and are now able to structure some real-world
financial instruments. The next tutorial will introduce ``Observations``, which are used to model
time-dependent market observables, such as stock prices and interest rates fixings.
