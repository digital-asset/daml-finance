.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Observations
############

Equity forward payoff
*********************

The payoff of financial derivatives depends on the performance of a certain underlying in the
future. As an example, consider an equity forward contract where a party agrees to purchase one
stock of Apple at a predetermined date and price in the future. The contract buyer will make a
profit if the market price of the stock is greater than the purchase price they pay (and make a loss
otherwise).

This payoff can be written as follows

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- FWD_CLAIM_SETUP_START
  :end-before: -- FWD_CLAIM_SETUP_END

In this example, the predetermined purchase price (strike price) is ``195 USD``.

``Observe "AAPL"`` is used to represent the time-dependent market price of Apple, which is unknown
at trade inception.

When lifecycling the claim at maturity, we must provide the observed market value for the
observable ``AAPL`` in order to resolve the claim's cashflows and other
`effects <#lifecycling-effect>`__.

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- FWD_OBS_START
  :end-before: -- FWD_OBS_END

This yields the expected payoff

.. code-block:: none

  "--- EFFECT on 2023-08-31 ---"
  "TARGET INSTRUMENT : MyClaim version 0"
  "RECEIVING"
  " => 5.0 USD"
  "GIVING"

You can change the stock's observed value in the script and see how this impacts the generated
cashflows.

As you might have realised, the multiplying factor within a ``scale`` builder does not have to be
a constant or deterministic quantity. It is a generic ``Observation``, which is a combination of
known amounts (built with ``Const``) and market observables (built with ``Observe``). These
building blocks can be combined together using standard algebraic operations (``+``, ``-``, ``*``
, ``/``).

In the example above, once the maturity date is reached the sub-claim
``scale (Observe "AAPL" - Const 195.0) $ one "USD"`` is acquired and the value of the observation
is looked up in the table for that maturity date.

Floating Rate Note
******************

There are cases where we want to explicitly specify the date at which a market observable is
evaluated. Take for example the case of a Floating Rate Note, which is a financial instrument
that pays a floating coupon based on an interest rate value observed a few days earlier.

An example of such payoff is the following

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- FRN_CLAIM_SETUP_START
  :end-before: -- FRN_CLAIM_SETUP_END

where

- we observe the value of the 3 month US Dollar LIBOR rate on ``10 Aug 2023``

- we pay a coupon for that rate on ``31 Aug 2023``

The ``ObserveAt`` observation builder is used to specify when the rate should be observed.
In order to lifecycle the claim at maturity we must include the rate observation in the table

.. literalinclude:: ../../finance-payoff-modeling/daml/Examples/BasicCombinators.daml
  :language: daml
  :start-after: -- FRN_OBS_START
  :end-before: -- FRN_OBS_END

which then yields to the expected payout

.. code-block:: none

  "--- EFFECT on 2023-08-31 ---"
  "TARGET INSTRUMENT : MyClaim version 0"
  "RECEIVING"
  " => 56300.0 USD"
  "GIVING"

Interest Rate Swap
******************

As an exercise, try to model

- a fixed-for-floating interest rate swap, where the claim owner
  receives coupons based on a floating rate in exchange for fixed rate coupons.

- a basis rate swap, where the owner receives coupons based on 3 month US Dollar LIBOR
  and pays coupons based on 6 month US Dollar LIBOR to their counterparty

Summary
*******

You now know how to setup payoffs containing complex time-dependent market observables.
You have the tools to model a large set of financial products, such as forwards and most interest
rate swap variations.
