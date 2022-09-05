.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Core Concepts
#############

This page describes the core concepts of the Daml Finance library. It also refers the reader to where each of these concepts is implemented in the library.

The most important definitions are also summarized in the :doc:`glossary <glossary>`.

Asset model
***********

The library’s asset model is the set of contracts that describe the financial rights and obligations that exist between parties. It is composed of instruments, holdings, and accounts.

It is important to note that the economic terms of an asset are separated from the representation of an asset holding. This is to allow centralized management of instruments (e.g. lifecycling), and to allow reuse of instruments and associated logic across different entities (e.g. custodians). It also avoids the data redundancy of replicating instrument data and logic on every holding contract.

All asset model interfaces are defined in the ``Daml.Finance.Interface.Asset`` package. Implementations are in ``Daml.Finance.Asset``.

Instrument
==========

An Instrument contract describes the economic terms (rights and
obligations) of one unit of a financial contract.

It can be as simple as an ISIN code referencing some real-world (off-ledger)
security, or it could encode specific on-ledger lifecycling logic.

Signatories
-----------

Every instrument must have an ``issuer`` party and a ``depository``
party, which are both signatories of the contract.

The terminology is borrowed from the real world, where an issuer of
e.g., a stock instrument deposits the paper certificate at a depository
and gets the corresponding amount credited in book-entry form.

On the ledger, the ``depository`` acts as a trusted party that prevents
the ``issuer`` from potentially acting maliciously.

Keys and versioning
-------------------

An instrument is identified by an ``Id``, which comprises a textual
label and a textual version.

It is usually referenced by key, the ``InstrumentKey`` comprising

-  the ``issuer``
-  the ``depository``
-  the ``id``

The version part of the ``Id`` can be used to keep track of the linear
evolution of an instrument.

For example, once a dividend on a share is paid, different versions
identify the cum-dividend and the ex-dividend share.

Implementations
---------------

A base implementation is provided in ``Daml.Finance.Asset``.

This template does not involve any lifecycling logic and is suitable to
model contracts that are likely to stay stable, such as currency
instruments.

The extensions packages provide additional business-specific
implementations, such as an ``Equity`` instrument (where the issuer can
pay dividends) or a ``Bond`` instrument (which includes coupon
payments).

The expectation is that customers define their own instruments suiting
the use-case they are modeling.

Holding
=======

A Holding contract represents the ownership of a certain amount of an
instrument by an owner at a custodian.

Where an instrument defines **what** a party holds, a holding defines
**how much** and **against which party** we are holding something.

.. _signatories-1:

Signatories
-----------

Every holding must have an ``owner`` party and a ``custodian`` party,
which are usually both signatories of the contract.

The terminology is again borrowed from the real-world: our cash or
shares are usually deposited at a custodian and we have (at least in
principle) the right to claim them back.

Properties of holdings
----------------------

A holding implementation can have specific properties such as being :ref:`fungible <fungibility>` or :ref:`transferable <transferability>`.

When, for instance, a holding is transferable, the owner has the right to transfer ownership to a different party at the same custodian.

These properties are exposed by implementing the corresponding interface (``Fungible`` and ``Transferable``, respectively).

.. _implementations-1:

Implementations
---------------

Implementations are provided in ``Daml.Finance.Asset`` for

-  a fungible and transferable holding
-  a holding which is transferable but not fungible
-  a holding which is neither transferable nor fungible

Account
=======

Finally, account contracts are used as proof of a relationship between a
``custodian`` and an asset ``owner``.

An ``owner`` must have an account contract with a ``custodian`` before a holding
contract can be created.

This is similar to how in the real world you need to open a bank account
before you can use the bank’s services.

In the library, accounts are used to prevent holding transfers to
unvetted third parties: Alice can transfer a holding to Bob only
if Bob has an account at the Bank (and has therefore been vetted
by the Bank).

.. _signatories-2:

Signatories
-----------

An account is co-signed by the account ``owner`` and the ``custodian``.

Keys
----

Accounts are keyed by an ``AccountKey``, which comprises

-  the account ``owner``
-  the account ``custodian``
-  a textual ``id``

.. _implementations-2:

Implementations
---------------

A base account implementation is provided in ``Daml.Finance.Asset``.

Settlement
**********

:ref:`Settlement <settlement>` refers to the execution of holding transfers originating from
a financial transaction.

For instance, an example FX spot transaction involves the transfer of a
EUR-denominated holding from Alice to Bob in exchange for a
USD-denominated holding.

The library provides facilities to execute these transfers atomically
(i.e., within the same Daml transaction) in the package ``Daml.Finance.Interface.Settlement``.

Step
====

The FX example transaction above contains two steps:

#. transfer EUR from Alice to Bob
#. transfer USD from Bob to Alice

They are represented using one ``Step`` each.
The step defines who is the sender, who is the receiver and what should be transferred (instrument and amount).

Instruction
===========

A ``Step`` is not sufficient to do a transfer. We also need to know exactly which holding should be used and to which account it should be transferred.
This is specified in an ``Instruction`` (one for each ``Step``).
The ``Instruction`` allows the sender to specify which holding to transfer, by exercising the ``Allocate`` choice.
The receiver can then specify which account should be used, by exercising the ``Approve`` choice.

Batch
=====

We could execute the transfer of the two instructions above individually, but that would cause
a problem if one instruction fails and the other one succeeds. Instead, we want to execute them
simultaneously in one atomic transaction. We can do that by using a ``Batch`` contract.

These settlement concepts are explained in more detail with example code in the :doc:`Settlement tutorial <tutorial/getting-started/settlement>`.

Lifecycling
***********

:ref:`Lifecycling <lifecycling>` is the evolution of instruments over their lifetime.
The library provides a standard mechanism for processing instruments accross asset types.

Various types of events result in cashflows and updated instrument definitions,
to reflect passed events and cashflows that have already been paid.

Events
======


The ``Event`` interface, which is defined in ``Daml.Finance.Interface.Lifecycle.Event`` is
used to handle different types of events:

Intrinsic
---------

Intrinsic events are contractual cash flows.
It is pre-defined in the contract terms exactly what triggers these events, for example:

- A certain date is reached, which results in a coupon payent of a bond. Time-based events are controlled using the ``DateClock`` template (not ledger time).
- The price of a stock reaches a certain level, resulting in a barrier hit. The relevant stock price is defined in an ``Observable`` template.

Extrinsic
---------

Extrinsic events, for example corporate actions and elections, are not pre-defined.
An external choice is exercised in order to trigger these events.

Instrument versions
===================

Consider a bond instrument which pays a fix coupon once a year. In this case, the
coupon payment is an intrinsic, time-based event.

When the bond has just been issued, the holder is entitled to all future coupon payments.
On the payment date of the first coupon, the bond is lifecycled by the issuer. This has
the following result:

#. The current coupon is paid to the holder of the bond.
#. The bond is replaced by a new version, which only includes the remaining coupons.

It is important to understand that there are two different instruments: one which includes the current coupon and one which does not.

Effects
=======

When an event is lifecycled, an ``Effect`` is produced. This is defined in ``Daml.Finance.Interface.Lifecycle.Effect``.
The ``Effect`` can be settled in order to produce the relevant cash flows and to create the new instrument version (reflecting the remaining cash flows).


These lifecycle concepts are explained in more detail with example code in the :doc:`Lifecycling tutorial <tutorial/getting-started/lifecycling>`.


