.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

How To Use the Option Extension Package
#######################################

To follow the script used in this tutorial, you can
`clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_. In particular,
the Option test folder ``src/test/daml/Daml/Finance/Instrument/Option/Test/`` is the starting point
of this tutorial.

How To Create an Option Instrument
**********************************

In order to create an option instrument, you first have to decide what type of option you need. The
:doc:`option extension package <../../packages/implementations/daml-finance-instrument-option>`
currently supports the following types of options:

European
========

The :ref:`European option <module-daml-finance-instrument-option-european-instrument-57671>`
instrument models cash-settled, auto exercising call or put options. For reference, European options
gives the holder the right, but not the obligation, to buy (in case of a Call) or to sell (in case
of a Put) the underlying asset at predetermined *strike* price on a specific *expiry* date in the
future.

As an example, we will create an option instrument that gives the holder the right to buy AAPL stock
at a given price. This example is taken from
``src/test/daml/Daml/Finance/Instrument/Option/Test/European.daml``, where all the details are
available.

We start by defining the terms:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Option/Test/European.daml
  :language: daml
  :start-after: -- CREATE_EUROPEAN_OPTION_VARIABLES_BEGIN
  :end-before: -- CREATE_EUROPEAN_OPTION_VARIABLES_END

If the close price of AAPL on the expiry date is above the *strike* price, the option holder would
profit from exercising the option and buying the stock at the strike price. Since this option type
is cash-settled, the value of the option would be *spot - strike* in the option currency.

On the other hand, if the close price of AAPL is below the *strike* price, the option would expire
worthless.

Now that we have defined the terms we can create the option instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Option/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_EUROPEAN_OPTION_INSTRUMENT_BEGIN
  :end-before: -- CREATE_EUROPEAN_OPTION_INSTRUMENT_END

Once the instrument is created, you can book a holding on it using
:ref:`Account.credit <module-daml-finance-interface-account-account-92922>`.

Frequently Asked Questions
**************************

How do I transfer or trade an option?
=====================================

When you have created a holding on an option instrument this can be transfered to another party.
This is described in the :doc:`Getting Started: Transfer <../getting-started/transfer>` tutorial.

In order to trade an option (transfer it in exchange for cash) you can also initiate a delivery
versus payment with atomic settlement. This is described in the
:doc:`Getting Started: Settlement <../getting-started/settlement>` tutorial.

How do I calculate settlement payments for an option?
=====================================================

On the expiry date, the issuer will need to lifecycle the European option. This will result in a
lifecycle effect for the payoff, which can be cash settled. This is described in detail in the
:doc:`Lifecycling <../getting-started/lifecycling>` and the
:doc:`Intermediated Lifecycling <intermediated-lifecycling>` tutorials.
