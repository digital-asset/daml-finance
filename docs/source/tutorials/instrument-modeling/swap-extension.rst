.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

How To Use the Swap Extension Package
#####################################

To follow the script used in this tutorial, you can
`clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_. In particular,
the Swap test folder ``src/test/daml/Daml/Finance/Instrument/Swap/Test/`` is the starting point of
this tutorial.

Prerequisites
*************

The Swap extension has many similarities with the Bond extension. This tutorial builds on the
:doc:`Bond Tutorial <bond-extension>`. Please check it out before reading the Swap specifics below.

How To Create a Swap Instrument
*******************************

There are different types of swaps, which differ both in the way regular payments are defined and
whether notional is exchanged. In order to create a swap instrument, you first have to decide what
type of swap you need. The
:doc:`swap extension package <../../packages/implementations/daml-finance-instrument-swap>`
currently supports the following types of swaps:

Interest Rate
=============

:ref:`Interest rate swap <module-daml-finance-instrument-swap-interestrate-instrument-86260>`
is the type of swap that shares most similarities with a bond. It has two legs:
one which pays a fix rate and another one which pays a floating rate. These rates are paid at the
end of every payment period.

As an example, we will create a swap instrument paying Libor 3M vs a 2.01% p.a. with a 3M payment
period. This example is taken from
``src/test/daml/Daml/Finance/Instrument/Swap/Test/InterestRate.daml``, where all the details are
available.

We start by defining the terms:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/InterestRate.daml
  :language: daml
  :start-after: -- CREATE_INTEREST_RATE_SWAP_VARIABLES_BEGIN
  :end-before: -- CREATE_INTEREST_RATE_SWAP_VARIABLES_END

The floating leg depends on a reference rate, which is defined by the *referenceRateId* variable.
The value of the reference rate is observed at the beginning of each payment period.

The *ownerReceivesFix* variable is used to specify whether a holding owner of this instrument
receives the fix or the floating leg.
This is not needed for bonds, because the regular payments are always in one direction (from the
issuer to the holder). However, in the case of a swap with two counterparties A and B, we need the
*ownerReceivesFix* variable to specify who receives fix and who receives floating. In this example,
the holding owner receives the floating leg.

Just as for bonds, we can use these variables to create a
:ref:`PeriodicSchedule <constr-daml-finance-interface-types-date-schedule-periodicschedule-99705>`:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Test/Util/Time.daml
  :language: daml
  :start-after: -- CREATE_PERIODIC_SCHEDULE_BEGIN
  :end-before: -- CREATE_PERIODIC_SCHEDULE_END

Note that this instrument only has one periodic schedule, which is used for both the fixed and the
floating leg.
It is also used for both the calculation period (to determine which floating rate to be used) and
the payment period (to determine when payments are done). The
:ref:`FpML swap template <fpml-swaps-tutorial-section>`
below offers more flexibility here. It has individual schedules, both for the fixed/floating leg and
for the calculation/payment periods. That would allow you to specify whether payments should be made
e.g. after each calculation period or only after every second calculation period.

Now that we have defined the terms we can create the swap instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_INTEREST_RATE_SWAP_INSTRUMENT_BEGIN
  :end-before: -- CREATE_INTEREST_RATE_SWAP_INSTRUMENT_END

Once this is done, you can create a holding on it using
:ref:`Account.credit <module-daml-finance-interface-account-account-92922>`.
The owner of the holding receives the floating leg (and pays the fix leg).

Currency
========

:ref:`Currency swaps <module-daml-finance-instrument-swap-currency-instrument-67721>`
are quite similar to interest rate swaps, except that the two legs are in different
currencies. Consequently, we need to create two cash instruments:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Currency.daml
  :language: daml
  :start-after: -- CREATE_CURRENCY_SWAP_CASH_INSTRUMENTS_BEGIN
  :end-before: -- CREATE_CURRENCY_SWAP_CASH_INSTRUMENTS_END

In the swap template they are referred to as *base currency* and *foreign currency*.

Here is an example of a fix vs fix currency swap: 3% p.a. in USD vs 2% p.a. in EUR with payments
every 3M:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Currency.daml
  :language: daml
  :start-after: -- CREATE_CURRENCY_SWAP_VARIABLES_BEGIN
  :end-before: -- CREATE_CURRENCY_SWAP_VARIABLES_END

In this example, the holding owner receives the foreign currency leg.

In order to calculate the interest rate payments, a notional is required in each currency. The
quantity of the holding refers to the notional of the base currency. The notional of the foreign
currency is defined as the quantity of the holding multiplied by the specified *fxRate*.

Note that this template is limited to fixed rates. It also does not support exchange of notionals.
If you need floating rates or exchange of notionals, please use the
:ref:`FpML swap template <fpml-swaps-tutorial-section>` below. It supports both of those features.

Here is how we create the currency swap instrument, using the two currencies defined above:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_CURRENCY_SWAP_INSTRUMENT_BEGIN
  :end-before: -- CREATE_CURRENCY_SWAP_INSTRUMENT_BEGIN

Once the instrument is created, you can create a holding on it. In our example, it the owner of the
holding receives the foreign currency leg (and pays the base currency leg).

Foreign Exchange
================

Despite the similarities in name,
:ref:`foreign exchange swaps <module-daml-finance-instrument-swap-foreignexchange-instrument-43394>`
(or FX swaps) are quite different from currency swaps.
An FX swap does not pay or receive interest. Instead, the two legs define an initial
FX transaction and a final FX transaction. Each transaction requires an FX rate and a transaction
date, which are predetermined between the counterparties.

The FX transactions involve two currencies. In the swap template these are referred to as *base
currency* and *foreign currency*. The convention is that the holding owner receives the foreign
currency in the initial transaction (and pays it in the final transaction).

Here is an example of an USD vs EUR FX swap. First, we define the two cash instruments:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/ForeignExchange.daml
  :language: daml
  :start-after: -- CREATE_FX_SWAP_CASH_INSTRUMENTS_BEGIN
  :end-before: -- CREATE_FX_SWAP_CASH_INSTRUMENTS_END

Then, we define the transaction dates and FX rates:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/ForeignExchange.daml
  :language: daml
  :start-after: -- CREATE_FX_SWAP_VARIABLES_BEGIN
  :end-before: -- CREATE_FX_SWAP_VARIABLES_END

The *firstPaymentDate* variable defines the date of the initial FX transaction. Generally, this is
on the issue date or shortly afterwards.

Finally, we create the FX swap instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_FOREIGN_EXCHANGE_SWAP_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FOREIGN_EXCHANGE_SWAP_INSTRUMENT_END

Once the instrument is created, you can create a holding on it. The owner of the holding receives the
foreign currency in the initial transaction. In the final transaction the sides are reversed.

Credit Default
==============

A :ref:`credit default swap <module-daml-finance-instrument-swap-creditdefault-instrument-88725>`
(CDS) pays a protection amount in case of a credit default event, in exchange
for a fix rate at the end of every payment period. The protection amount is defined as
*1-recoveryRate*. The *recoveryRate* is defined as the amount recovered when a borrower defaults,
expressed as a percentage of notional.

If a credit event occurs, the swap expires after the protection amount has been paid, i.e., no more
rate payments are required afterwards.

Here is an example of a CDS that pays *1-recoveryRate* in the case of a default on TSLA bonds:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/CreditDefault.daml
  :language: daml
  :start-after: -- CREATE_CREDIT_DEFAULT_SWAP_VARIABLES_BEGIN
  :end-before: -- CREATE_CREDIT_DEFAULT_SWAP_VARIABLES_END

In our example, the issuer pays the protection leg of the swap.

As you can see in this example, two observables are required for a CDS:

#. *defaultProbabilityReferenceId*: The reference ID of the default probability observable. For
   example, in case of protection against a "TSLA bond payment default" this should be a valid
   reference to the "TSLA default probability".
#. *recoveryRateReferenceId*: The reference ID of the recovery rate observable. For example, in case
   of a "TSLA bond payment default with a 60% recovery rate" this should be a valid reference to the
   "TSLA bond recovery rate".

Finally, we create the CDS instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_CREDIT_DEFAULT_SWAP_INSTRUMENT_BEGIN
  :end-before: -- CREATE_CREDIT_DEFAULT_SWAP_INSTRUMENT_END

Once the instrument is created, you can create a holding on it. In our example, the owner of the
holding receives the protection leg (and pays the fix leg).

Asset
=====

An :ref:`asset swap <module-daml-finance-instrument-swap-asset-instrument-28127>`
is a general type of swap with two legs: one which pays a fix rate and another one
which pays the performance of an asset. It can be used to model:

* equity swaps
* some types of commodity swaps (of the form *performance vs rate*)
* other swaps with the same payoff on other asset types.

Here is an example of an asset swap that pays AAPL total return vs 2.01% fix p.a., payment every 3M:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Asset.daml
  :language: daml
  :start-after: -- CREATE_ASSET_SWAP_VARIABLES_BEGIN
  :end-before: -- CREATE_ASSET_SWAP_VARIABLES_END

In our example, the issuer pays the asset leg of the swap.

One observable is required: *referenceAssetId*. The template calculates the performance for each
payment period using this observable. Performance is calculated from the start date to the end date
of each payment period. The reference asset Observable needs to contain the appropriate type of
fixings:

* *unadjusted* fixings in case of a *price return* asset swap
* *adjusted* fixings in case of a *total return* asset swap

Finally, we create the asset swap instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_ASSET_SWAP_INSTRUMENT_BEGIN
  :end-before: -- CREATE_ASSET_SWAP_INSTRUMENT_END

Once this is done, you can create a holding on it.
The owner of the holding receives the asset leg (and pays the fix leg).

.. _fpml-swaps-tutorial-section:

FpML
====

Unlike the other swap types above, the
:ref:`FpML swap <module-daml-finance-instrument-swap-fpml-instrument-17241>` template is
not a new type of payoff. Instead, it allows you to input other types of swaps using the
`FpML schema <https://www.fpml.org/spec/fpml-5-11-3-lcwd-1/html/confirmation/schemaDocumentation/schemas/fpml-ird-5-11_xsd/complexTypes/Swap.html>`_.
Currently, interest rate swaps and currency swaps are supported.
The template can quite easily be extended to FX swaps.

Specifically, it allows you to specify one
`swapStream <https://www.fpml.org/spec/fpml-5-11-3-lcwd-1/html/confirmation/schemaDocumentation/schemas/fpml-ird-5-11_xsd/complexTypes/Swap/swapStream.html>`_
object for each leg of the swap.

We start by defining the general terms:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Fpml.daml
  :language: daml
  :start-after: -- CREATE_FPML_SWAP_VARIABLES_BEGIN
  :end-before: -- CREATE_FPML_SWAP_VARIABLES_END

The *issuerPartyRef* and the *clientPartyRef* variables are used to specify who pays each leg (see
*payerPartyReference* below).

The fixed leg of the swap can now be defined using Daml data types that correspond to the
`swapStream <https://www.fpml.org/spec/fpml-5-11-3-lcwd-1/html/confirmation/schemaDocumentation/schemas/fpml-ird-5-11_xsd/complexTypes/Swap/swapStream.html>`_
schema:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Fpml.daml
  :language: daml
  :start-after: -- CREATE_FPML_SWAP_FIX_LEG_BEGIN
  :end-before: -- CREATE_FPML_SWAP_FIX_LEG_END

As you can see, the
:ref:`Daml SwapStream data type <type-daml-finance-interface-instrument-swap-fpml-fpmltypes-swapstream-38811>`
matches the `swapStream FpML schema <https://www.fpml.org/spec/fpml-5-11-3-lcwd-1/html/confirmation/schemaDocumentation/schemas/fpml-ird-5-11_xsd/complexTypes/Swap/swapStream.html>`_.
Please note that the actual parsing from FpML to Daml is not done by this template. It has to be
implemented on the client side.

Similarly, the floating leg of the swap is defined like this:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Fpml.daml
  :language: daml
  :start-after: -- CREATE_FPML_SWAP_FLOAT_LEG_BEGIN
  :end-before: -- CREATE_FPML_SWAP_FLOAT_LEG_END

There are three main ways to define which interest rate should be used for a stub period. They are
all included in the fix or floating leg above, either in the inital or in the final stub period. In
short, it depends on the content of
:ref:`StubCalculationPeriodAmount <type-daml-finance-interface-instrument-swap-fpml-fpmltypes-stubcalculationperiodamount-23577>`:

#. *None*: No special stub rate is provided. Instead, use the same rate as was specified in the
   corresponding
   :ref:`Calculation <type-daml-finance-interface-instrument-swap-fpml-fpmltypes-calculation-37694>`.
#. Specific *stubRate*: Use this specific fix rate.
#. Specific *floatingRate*: Use this specific floating rate (if one rate is provided). If two rates
   are provided: use linear interpolation between the two rates.

Finally, we create the FpML swap instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Swap/Test/Util.daml
  :language: daml
  :start-after: -- CREATE_FPML_SWAP_INSTRUMENT_BEGIN
  :end-before: -- CREATE_FPML_SWAP_INSTRUMENT_END

Once this is done, you can create a holding on it. In this particular example trade, the
notional is specified in the FpML instrument. This means that you would only book a unit holding
(quantity=1.0) on the instrument.

Frequently Asked Questions
**************************

Why do the swaps have an issuer?
================================

In the case of bonds, the instrument has a well-defined issuer. This is not necessarily the case for
swaps, where two counterparties A and B swap the payments associated with each leg. However, in
practice one of the counterparties is often a swap dealer, who shares some of the characteristics of
a bond issuer. For the purpose of lifecycling in Daml Finance, we require one of the counterparties
to take the role as issuer. This counterparty will serve as calculation agent and provide the
observables required to calculate the swap payments.

The documentation of the Daml Finance asset model contains an
:ref:`OTC swap example <otc-swap-asset-model>`.
