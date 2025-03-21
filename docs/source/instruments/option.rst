.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Option Instrument
#################

To follow the script used in this tutorial, you can
`clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_. In particular,
the Option test folder
`Instrument/Option/Test/ <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Option/Test/>`_
is the starting point of this tutorial.

How To Create an Option Instrument
**********************************

In order to create an option instrument, you first have to decide what type of option you need. The
:doc:`option extension package <../packages/implementations/daml-finance-instrument-option>`
currently supports the following types of options:

European Options
================

European options give the holder the right, but not the obligation, to buy (in case of a *call*) or
to sell (in case of a *put*) the underlying asset at predetermined *strike* price on a specific
*expiry* date in the future.

Daml Finance supports two types of European Options:

.. _european-physical:

Physically settled European Option
----------------------------------

The
:ref:`EuropeanPhysical <module-daml-finance-instrument-option-v0-europeanphysical-instrument-57525>`
instrument models physically settled call or put options.

There are two important characteristics of this instrument:

#. *physical settlement*: option holders that choose to exercise will buy (in case of a *call*) or
   sell (in case of a *put*) the underlying asset at the predetermined *strike* price. Since this
   option instrument is physically settled, it means that the underlying asset will change hands.
#. *manual exercise*: This option instrument is not automatically exercised. Instead, the option
   holder must manually decide whether or not to exercise. This is done by making an *Election*.

As an example, consider an option instrument that gives the holder the right to buy AAPL stock
at a given price. This example is taken from
`Instrument/Option/Test/EuropeanPhysical.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Option/Test/EuropeanPhysical.daml>`_
, where all the details are available. Also, Check out the
:ref:`Election based lifecycling tutorial <election-based-lifecycling>` for more details on how
how to define and process an *Election* in practice.

You start by defining the terms:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Option/Test/EuropeanPhysical.daml
  :language: daml
  :start-after: -- CREATE_EUROPEAN_PHYSICAL_OPTION_VARIABLES_BEGIN
  :end-before: -- CREATE_EUROPEAN_PHYSICAL_OPTION_VARIABLES_END

Now that the terms have been defined, you can create the option instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Option/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_EUROPEAN_PHYSICAL_OPTION_INSTRUMENT_BEGIN
  :end-before: -- CREATE_EUROPEAN_PHYSICAL_OPTION_INSTRUMENT_END

Once this is done, you can create a holding on it using
:ref:`Account.Credit <module-daml-finance-interface-account-v4-account-30007>`.

Cash-settled European Option
----------------------------

The  :ref:`EuropeanCash <module-daml-finance-instrument-option-v0-europeancash-instrument-35799>`
instrument models cash-settled, auto-exercising call or put options. They are similar to the
:ref:`EuropeanPhysical <module-daml-finance-instrument-option-v0-europeancash-instrument-35799>`
instrument described above, but there are two important differences:

#. *cash settlement*: This means that the underlying asset will not change hands. Instead, a cash
   settlemement will take place (if the option is exercised).
#. *automatic exercise*: This option instrument is automatically exercised. No manual *Election* is
   required by the holder.

As an example, consider an option instrument that gives the holder the right to buy AAPL stock
at a given price. This example is taken from
`Instrument/Option/Test/EuropeanCash.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Option/Test/EuropeanCash.daml>`_
, where all the details are available.

You start by defining the terms:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Option/Test/EuropeanCash.daml
  :language: daml
  :start-after: -- CREATE_EUROPEAN_OPTION_VARIABLES_BEGIN
  :end-before: -- CREATE_EUROPEAN_OPTION_VARIABLES_END

Now that the terms have been defined, you can create the option instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Option/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_EUROPEAN_OPTION_INSTRUMENT_BEGIN
  :end-before: -- CREATE_EUROPEAN_OPTION_INSTRUMENT_END

Once this is done, you can create a holding on it using
:ref:`Account.Credit <module-daml-finance-interface-account-v4-account-30007>`.

If the close price of AAPL on the expiry date is above the *strike* price, the option holder would
profit from exercising the option and buying the stock at the strike price. The value of the option
would be *spot - strike*. Since this option type is cash-settled, this amount would be paid in the
option currency after lifecycling.

On the other hand, if the close price of AAPL is below the *strike* price, the option would expire
worthless.

This option instrument is automatically exercised. This means that the decision whether or not to
exercise is done automatically by comparing the *strike* price to an observation of the close price.
For this to work, you need to define an *Observation* as well:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Option/Test/EuropeanCash.daml
  :language: daml
  :start-after: -- CREATE_EUROPEAN_OPTION_OBSERVATIONS_BEGIN
  :end-before: -- CREATE_EUROPEAN_OPTION_OBSERVATIONS_END

Barrier Option
==============

The
:ref:`BarrierEuropeanCash <module-daml-finance-interface-instrument-option-v0-barriereuropeancash-instrument-35570>`
instrument models barrier options. They are similar to the
:ref:`EuropeanCash <module-daml-finance-instrument-option-v0-europeancash-instrument-35799>`
instrument described above, but also contain a barrier that is used to activate (or, alternatively,
knock out) the option. The
:ref:`BarrierTypeEnum <type-daml-finance-interface-instrument-option-v0-types-barriertypeenum-77029>`
describes which barrier types are supported.

As an example, consider an option instrument that gives the holder the right to buy AAPL stock
at a given price. However, if AAPL ever trades at or below a given barrier level, the option is
knocked out (which means that it expires worthless). In other words, this describes a
:ref:`DownAndOut <constr-daml-finance-interface-instrument-option-v0-types-downandout-96532>` option.
This example is taken from
`Instrument/Option/Test/BarrierEuropeanCash.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Option/Test/BarrierEuropeanCash.daml>`_
, where all the details are available.

You start by defining the terms:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Option/Test/BarrierEuropeanCash.daml
  :language: daml
  :start-after: -- CREATE_BARRIER_EUROPEAN_OPTION_VARIABLES_BEGIN
  :end-before: -- CREATE_BARRIER_EUROPEAN_OPTION_VARIABLES_END

Now that the terms have been defined, you can create the option instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Option/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_BARRIER_EUROPEAN_OPTION_INSTRUMENT_BEGIN
  :end-before: -- CREATE_BARRIER_EUROPEAN_OPTION_INSTRUMENT_END

Once this is done, you can create a holding on it using
:ref:`Account.Credit <module-daml-finance-interface-account-v4-account-30007>`.

Compared to the
:ref:`EuropeanCash option <module-daml-finance-instrument-option-v0-europeancash-instrument-35799>`
this instrument needs to be lifecycled not only at expiry but also during its lifetime in case of a
barrier hit. This is done in the same way as lifecycling at maturity, i.e. an *Observation* is
provided for the reference asset identifier, containing the date and the underlying price.

Dividend Option
===============

The :ref:`Dividend <module-daml-finance-instrument-option-v0-dividend-instrument-16416>`
instrument models physically settled, manually exercised dividend options. For reference, a
dividend option gives the holder the right to choose one out of several dividend payouts, on a
specific *expiry* date in the future. The following payout types are supported:

#. *Cash*: The dividend is paid in cash. This a mandatory option. In addition, the issue can offer:
#. *Shares*: The dividend is paid in shares. To the investor this is similar to a
   :ref:`Bonus Issue <bonus-issue>`.
#. *CashFx*: The dividend is paid in cash in a foreign currency.

As an example, consider an option instrument that gives the holder the right to choose to receive
AAPL dividends either as cash, shares or cash in a foreign currency (EUR). This example is taken
from
`Instrument/Option/Test/Dividend.daml <https://github.com/digital-asset/daml-finance/blob/main/src/test/daml/Daml/Finance/Instrument/Option/Test/Dividend.daml>`_
, where all the details are available.

You start by defining the terms:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Option/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_DIVIDEND_OPTION_VARIABLES_BEGIN
  :end-before: -- CREATE_DIVIDEND_OPTION_VARIABLES_END

Now that the terms have been defined, you can create the option instrument:

.. literalinclude:: ../src/test/daml/Daml/Finance/Instrument/Option/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_DIVIDEND_OPTION_INSTRUMENT_BEGIN
  :end-before: -- CREATE_DIVIDEND_OPTION_INSTRUMENT_END

Once this is done, you can create a holding on it using
:ref:`Account.Credit <module-daml-finance-interface-account-v4-account-30007>`.

On the expiry date, the option holder will make an *Election* out of the available choices. The
lifecycling of this option works in the same way as for
:ref:`physically settled European options <european-physical>`.

Frequently Asked Questions
**************************

How do I transfer or trade an option?
=====================================

When you have created a holding on an option instrument this can be transferred to another party.
This is described in the :doc:`Getting Started: Transfer <../tutorials/getting-started/transfer>`
tutorial.

In order to trade an option (transfer it in exchange for cash) you can also initiate a delivery
versus payment with atomic settlement. This is described in the
:doc:`Getting Started: Settlement <../tutorials/getting-started/settlement>` tutorial.

How do I calculate settlement payments for an option?
=====================================================

On the expiry date, the issuer will need to lifecycle the European option. This will result in a
lifecycle effect for the payoff, which can be cash settled. This is described in detail in the
:doc:`Lifecycling <../tutorials/getting-started/lifecycling>` and the
:doc:`Intermediated Lifecycling <../tutorials/advanced-topics/lifecycling/intermediated-lifecycling>`
tutorials.
