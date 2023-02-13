.. Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
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
initialized. This creates the necessary factories. Your app can then create equity instruments
through these factory interfaces.

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
the ``DeclareDividend`` choice. It creates a
:ref:`Distribution Event <module-daml-finance-lifecycle-event-distribution-17302>`,
which allows you to specify distribution per share. In the case of a cash dividend, this would be a
cash instrument. However, the company can also choose to distribute additional stock or even stock
options. Since the
:ref:`Distribution Event <module-daml-finance-lifecycle-event-distribution-17302>`
supports an arbitrary ``perUnitDistribution`` instrument, it can be used to model those use cases as
well.

In order to process a lifecycle event, you have to create two versions of the instrument:
one before the event and one after the event. In the case of a dividend event, this means
one instrument *cum* dividend (which includes the dividend) and one *ex* dividend (which does no
longer include the dividend):

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_INSTRUMENTS_BEGIN
  :end-before: -- CREATE_EQUITY_INSTRUMENTS_END

Once this is done, you can create a holding on it. This is not limited to integer
holdings, but fractional holdings are supported as well:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_HOLDING_BEGIN
  :end-before: -- CREATE_EQUITY_HOLDING_END

We create a distribution rule for the cash dividend. It defines the business logic for the dividend
and it has the issuer as signatory:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_DISTRIBUTION_RULE_BEGIN
  :end-before: -- CREATE_EQUITY_DISTRIBUTION_RULE_END

We also need a distribution event, which defines the terms of the dividend. In this case, it is
USD 2 cash per share (this also works for a fractional amount of shares):

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_DISTRIBUTION_EVENT_BEGIN
  :end-before: -- CREATE_EQUITY_DISTRIBUTION_EVENT_END

This allows the issuer to lifecycle the instrument by exercising the ``Evolve`` choice:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- LIFECYCLE_CASH_DIVIDEND_BEGIN
  :end-before: -- LIFECYCLE_CASH_DIVIDEND_END

This results in a lifecycle effect, which can be settled. The settlement of effects is covered
in the :doc:`Lifecycling tutorial <../getting-started/lifecycling>`.

Bonus issue
***********

Instead of a cash dividend, a company may also decide to offer free shares (or warrants) instead of
cash to current shareholders. This is called *bonus issue* and it is modeled in a similar way
to the *dividend* above. The main difference is in the distribution event, which now distributes a
different instrument (equity instead of cash):

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_BONUS_ISSUE_DISTRIBUTION_EVENT_BEGIN
  :end-before: -- CREATE_EQUITY_BONUS_ISSUE_DISTRIBUTION_EVENT_END

Similarly, if there is a bonus issue that awards warrants instead of equity, that can be modeled in
the same way. Just replace the equity instrument by a warrant instrument on the
``perUnitDistribution`` line above.

Dividend option
***************

A company may give shareholders the option of choosing what kind of dividend they want to
receive. For example, a shareholder could choose between a dividend in cash *or* in stock.
The ``DeclareDividend`` choice can be used for this as well. The issuer creates one event for each
dividend option that shareholders can choose from:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_DIVIDEND_OPTION_DISTRIBUTION_EVENT_BEGIN
  :end-before: -- CREATE_EQUITY_DIVIDEND_OPTION_DISTRIBUTION_EVENT_END

The issuer then lifecycles each event individually, to generate two alternative lifecycling effects:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- LIVECYCLE_DIVIDEND_OPTION_BEGIN
  :end-before: -- LIVECYCLE_DIVIDEND_OPTION_END

The investor can then claim one or the other:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Dividend.daml
  :language: daml
  :start-after: -- INVESTOR_CLAIMS_DIVIDEND_OPTION_BEGIN
  :end-before: -- INVESTOR_CLAIMS_DIVIDEND_OPTION_END

When this is settled, the investor's holding is consumed, which prevents the investor from receiving
more than one of the dividend options.

Rights Issue
************

In order to raise money, a company may decide to give current shareholders the right (but not the
obligation) to purchase additional shares at a discounted price. This can be modeled using two
components:

- An option instrument, which describes the economic term of the rights a shareholder receives.
  For example, this could be a European option with a strike price below the current spot price, and
  a maturity three weeks in the future.
  The :doc:`Generic Tutorial <generic-extension>` describes how to create a European option.
- The ``DeclareDividend`` choice to distribute the above option instrument in the correct proportion
  (e.g. 3 option contracts for each 10 shares held). This can be done in the same way as the Bonus
  Issue example described earlier, just change the ``perUnitDistribution`` line to distribute the
  option instrument you created above.

When current shareholders receive the option instrument they can typically choose between:

#. Exercising the option. The :doc:`Generic Tutorial <generic-extension>` describes how to elect to
   exercise the option.
#. Choosing not to exercise the option. The option will expire worthless.
#. Selling the option. This is not always possible, it depends on the terms of the rights issue.
   :doc:`Getting Started: Settlement <../getting-started/settlement>` describes how this could be
   done.


Stock split
***********

A stock split is when a company increases its number of shares. For example, a 2-for-1 stock split
means that a shareholder will have two shares after the split for every share held before the split.
This is modeled using the ``DeclareStockSplit`` choice, which has an ``adjustmentFactor`` argument.

The ``DeclareStockSplit`` choice creates a
:ref:`Replacement Event <module-daml-finance-lifecycle-event-replacement-51859>`,
which allows you to replace units of an instrument with another instrument (or a basket of other
instruments). Consequently, this interface can also be used for other types of corporate actions
(for example, see the *merger* scenario below).

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

This allows the issuer to lifecycle the instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/StockSplit.daml
  :language: daml
  :start-after: -- LIFECYCLE_STOCK_SPLIT_BEGIN
  :end-before: -- LIFECYCLE_STOCK_SPLIT_END

This results in a lifecycle effect, which can be settled (similar to the *dividend* scenario above).

Reverse Stock Split
===================

The stock split described above increases the number of shares available. Alternatively, a company
may also decide to *decrease* the number of shares. This is referred to as *reverse stock split* or
*stock consolidation*.

The ``DeclareStockSplit`` choice supports this as well. For example, for a 1-for-10 reverse split,
modify the ``adjustmentFactor`` to 10/1 = 10.0 in the example above.

Merger
******

The merger scenario models the case when one company acquires another company and pays for it using
its own shares. This is modeled using the ``DeclareReplacement`` choice, which also uses the
:ref:`Replacement Event <module-daml-finance-lifecycle-event-replacement-51859>`
(like the *stock split* scenario above).
This is a mandatory exchange offer: no election is required (or possible) by the shareholder.

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
factor used in ``perUnitReplacement`` is 0.5:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Merger.daml
  :language: daml
  :start-after: -- CREATE_EQUITY_REPLACEMENT_EVENT_BEGIN
  :end-before: -- CREATE_EQUITY_REPLACEMENT_EVENT_END

This allows the issuer to lifecycle the instrument:

.. literalinclude:: ../../../../src/test/daml/Daml/Finance/Instrument/Equity/Test/Merger.daml
  :language: daml
  :start-after: -- LIFECYCLE_MERGER_BEGIN
  :end-before: -- LIFECYCLE_MERGER_END

This results in a lifecycle effect, which can be settled as usual.

Frequently Asked Questions
**************************

How do I transfer or trade an Equity?
=====================================

When you have created a holding on an Equity instrument this can be transfered to another party.
This is described in the :doc:`Getting Started: Transfer <../getting-started/transfer>` tutorial.

In order to trade an Equity (transfer it in exchange for cash) you can also initiate a delivery
versus payment with atomic settlement. This is described in the
:doc:`Getting Started: Settlement <../getting-started/settlement>` tutorial.

How do I process dividend payments for an Equity?
=================================================

On the dividend payment date, the issuer will need to lifecycle the Equity. This will result in a
lifecycle effect for the dividend, which can be cash settled. This is described in detail in the
:doc:`Lifecycling <../getting-started/lifecycling>` and the
:doc:`Intermediated Lifecycling <intermediated-lifecycling>` tutorials (depending on what kind of
settlement you need).
