.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

How to Use the Equity Extension Package
#######################################

To follow the script used in this tutorial, you can
`clone the Daml Finance repository <https://github.com/digital-asset/daml-finance>`_. In particular,
the Equity test folder ``src/test/daml/Daml/Finance/Instrument/Equity/Test/`` is the starting point
of this tutorial.

How to Use the Equity Extension in Your Application
***************************************************

As explained in the :ref:`Getting Started <structure-of-code-dependencies>` section and on the
:doc:`Architecture <../../overview/architecture>` page, your app should only depend on the interface
layer of Daml Finance. For equities this means that you should only include the
:doc:`equity interface package <../../packages/interfaces/daml-finance-interface-instrument-equity>`.

Your initialization scripts are an exception, since they are only run once when your app is
initialized. This creates the necessary factories. Your app can then create Equitys through these
factory interfaces.

The Equity Interface
********************

The equity extension supports different lifecycle related events, for example dividends, stock
splits and mergers. These are modeled using the choices on the
:ref:`Equity interface <module-daml-finance-interface-instrument-equity-instrument-13224>`,
namely ``DeclareDividend``, ``DeclareReplacement`` and ``DeclareStockSplit``. We will now
demonstrate each one with a concrete lifecycle event.

Dividend
********

The most common lifecycle event of an equity is probably dividends. This normally means that
the holder of a stock receives a given amount of cash for each stock held. This is modeled using
the ``DeclareDividend`` choice. It implements the
:ref:`Distribution Event interface <module-daml-finance-interface-lifecycle-event-distribution-91943>`,
which allows you to specify distribution of units of an instrument for each unit of a target
instrument. In the case of a cash dividend, this would be a cash instrument. However, the
company can also choose to distribute additional stock or even stock options. The
:ref:`Distribution Event implementation <module-daml-finance-lifecycle-event-distribution-17302>`
supports an arbitrary instrument to model those use cases.

In order to process a lifecycle event, you have to create two versions of the instrument:
one before the event and one after the instrument. In the case of a dividend event, this means
one instrument *cum* dividend (which includes the dividend) and one *ex* dividend (which does no
longer include the dividend):

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_INSTRUMENTS_BEGIN
  :end-before: -- CREATE_EQUITY_INSTRUMENTS_END

We create a distribution rule for the cash dividend:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_DISTRIBUTION_RULE_BEGIN
  :end-before: -- CREATE_EQUITY_DISTRIBUTION_RULE_END

We also need a distribution event:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_DISTRIBUTION_EVENT_BEGIN
  :end-before: -- CREATE_EQUITY_DISTRIBUTION_EVENT_END

This allows us to lifecycle the instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- LIFECYCLE_CASH_DIVIDEND_BEGIN
  :end-before: -- LIFECYCLE_CASH_DIVIDEND_END

This results in a lifecycle effect, which can be settled. The settlement of effects is covered
in the :doc:`Lifecycling tutorial <../getting-started/lifecycling>`.

Stock split
***********

A stock split is when a company increases its number of shares. For example, a 2-for-1 stock split
means that a shareholder will have two shares after the split for every share held before the split.
This is modeled using the ``DeclareStockSplit`` choice, which has an ``adjustmentFactor`` argument.

The ``DeclareStockSplit`` choice implements the
:ref:`Replacement Event interface <module-daml-finance-interface-lifecycle-event-replacement-2440>`,
which allows you to replace units of an instrument with another instrument (or a basket of other
instruments). Consequently, this interface can also be used for other types of corporate actions
(for example, see the Merger scenario below).

The workflow for a stock split is quite similar to that of a dividend above. We start by defining
the instrument before and after the lifecycle event:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/StockSplit.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_INSTRUMENTS_BEGIN
  :end-before: -- CREATE_EQUITY_INSTRUMENTS_END

We create a replacement rule for the stock split:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/StockSplit.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_REPLACEMENT_RULE_BEGIN
  :end-before: -- CREATE_EQUITY_REPLACEMENT_RULE_END

We also need a replacement event. For a 2-for-1 stock split, the ``adjustmentFactor`` is 1/2 = 0.5:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/StockSplit.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_REPLACEMENT_EVENT_BEGIN
  :end-before: -- CREATE_EQUITY_REPLACEMENT_EVENT_END

This allows us to lifecycle the instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/StockSplit.daml
  :language: daml
  :start-after: -- LIFECYCLE_STOCK_SPLIT_BEGIN
  :end-before: -- LIFECYCLE_STOCK_SPLIT_END

This results in a lifecycle effect, which can be settled (similar to the dividend scenario above).

Merger
******

The merger scenario models the case when one company acquires another company and pays for it using
its own shares. This is modeled using the ``DeclareReplacement`` choice, which also uses the
:ref:`Replacement Event interface <module-daml-finance-interface-lifecycle-event-replacement-2440>`
(like the stock split scenario above).

We start by defining the instrument before and after the merger. Shares of company ABC are being
replaced by shares of company XYZ:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Merger.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_INSTRUMENTS_BEGIN
  :end-before: -- CREATE_EQUITY_INSTRUMENTS_END

We create a replacement rule for the merger:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Merger.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_REPLACEMENT_RULE_BEGIN
  :end-before: -- CREATE_EQUITY_REPLACEMENT_RULE_END

We also need a replacement event. Two shares of ABC are replaced by one share of XYZ, so the
factor in ``perUnitReplacement`` is 0.5:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Merger.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_REPLACEMENT_EVENT_BEGIN
  :end-before: -- CREATE_EQUITY_REPLACEMENT_EVENT_END

This allows us to lifecycle the instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Merger.daml
  :language: daml
  :start-after: -- LIFECYCLE_MERGER_BEGIN
  :end-before: -- LIFECYCLE_MERGER_END

This results in a lifecycle effect, which can be settled as usual.



Frequently Asked Questions
**************************

How do I transfer or trade an Equity?
=====================================

When you have created a holding on a Equity instrument this can be transfered to another party.
This is described in the :doc:`Getting Started: Transfer <../getting-started/transfer>` tutorial.

In order to trade a Equity (transfer it in exchange for cash) you can also initiate a delivery versus
payment with atomic settlement. This is described in the
:doc:`Getting Started: Settlement <../getting-started/settlement>` tutorial.

How do I process dividend payments for a Equity?
================================================

On the coupon payment date, the issuer will need to lifecycle the Equity. This will result in a
lifecycle effect for the coupon, which can be cash settled. This is described in detail in the
:doc:`Lifecycling <../getting-started/lifecycling>` and the
:doc:`Intermediated Lifecycling <intermediated-lifecycling>` tutorials.
