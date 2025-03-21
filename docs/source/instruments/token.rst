.. Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Token Instrument
################

A Token is a simple instrument template whose economic terms on the ledger are defined by two
textual fields, namely an ``id`` and a ``description``.

It is often used to model financial instruments that do not exhibit complex lifecycling logic,
such as currencies.

How to create a Token Instrument
********************************

The following code snippets are taken from the
:doc:`Getting Started tutorial <../tutorials/getting-started/intro>`, which you can install using
the Daml assistant.

In order to instantiate a Token Instrument, we first need to create the corresponding instrument
factory template

.. literalinclude:: ../quickstart-finance/daml/Scripts/Holding.daml
  :language: daml
  :start-after: -- CREATE_INSTRUMENT_FACTORY_BEGIN
  :end-before: -- CREATE_INSTRUMENT_FACTORY_END

We can then specify the terms of the instrument and exercise the ``Create`` choice in the
factory to create the token.

.. literalinclude:: ../quickstart-finance/daml/Scripts/Holding.daml
  :language: daml
  :start-after: -- ISSUE_CASH_INSTRUMENT_BEGIN
  :end-before: -- ISSUE_CASH_INSTRUMENT_END

How to lifecycle a Token Instrument
***********************************

Generic corporate actions, such as
:ref:`Distribution events <module-daml-finance-lifecycle-v4-event-distribution-38493>`, can be
applied to Token Instruments.
The :doc:`Lifecycling <../tutorials/getting-started/lifecycling>`
section of the Getting Started tutorial shows how this is done in detail.

Frequently Asked Questions
**************************

How do I transfer or trade a Token Instrument?
==============================================

When you have created a holding on a token instrument this can be transferred to another party.
This is described in the
:doc:`Getting Started: Transfer <../tutorials/getting-started/transfer>` tutorial.

In order to trade a token (transfer it in exchange for cash) you can also initiate a delivery versus
payment with atomic settlement. This is described in the
:doc:`Getting Started: Settlement <../tutorials/getting-started/settlement>` tutorial.
