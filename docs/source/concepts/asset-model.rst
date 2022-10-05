.. Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Asset Model
###########

The library’s asset model is the set of contracts that describe the financial rights and obligations that exist between parties. It is composed of instruments, holdings, and accounts.

It is important to note that the economic terms of an asset are separated from the representation of an asset holding. This allows centralized management of instruments (e.g. lifecycling) and the reuse of instruments and associated logic across different entities (e.g. custodians). It also avoids the data redundancy of replicating instrument data and logic on every holding contract.

All asset model interfaces are defined in the ``Daml.Finance.Interface.Holding`` package. Implementations are in ``Daml.Finance.Holding``.

Instrument
**********

An instrument contract describes the economic terms (rights and
obligations) of one unit of a financial contract.

It can be as simple as an ISIN code referencing some real-world (off-ledger)
security, or it can encode specific on-ledger lifecycling logic.

Signatories
===========

Every instrument must have an ``issuer`` party and a ``depository``
party, which are both signatories of the contract.

The terminology is borrowed from the real world, where an issuer of
e.g., a stock instrument deposits the paper certificate at a depository
and gets the corresponding amount credited in book-entry form.

On the ledger, the ``depository`` acts as a trusted party that prevents
the ``issuer`` from potentially acting maliciously.

Keys and Versioning
===================

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
===============

A base implementation is provided in ``Daml.Finance.Instrument.Token``.

This template does not involve any lifecycling logic and is suitable to
model contracts that are likely to stay stable, such as currency
instruments.

The extensions packages provide additional business-specific
implementations, such as an ``Equity`` instrument (where the issuer can
pay dividends) or a ``Bond`` instrument (which includes coupon
payments).

The expectation is that customers define their own instruments suiting
the use case they are modeling.

Holding
*******

A holding contract represents the ownership of a certain amount of an
instrument by an owner at a custodian.

Where an instrument defines **what** a party holds, a holding defines
**how much** and **against which party** we are holding something.

.. _signatories-1:

Signatories
===========

Every holding must have an ``owner`` party and a ``custodian`` party,
which are usually both signatories of the contract.

The terminology is again borrowed from the real world: our cash or
shares are usually deposited at a custodian and we have (at least in
principle) the right to claim them back.

Properties of Holdings
======================

A holding implementation can have specific properties such as being :ref:`fungible <fungibility>` or :ref:`transferable <transferability>`.

When, for instance, a holding is transferable, the owner has the right to transfer ownership to a different party at the same custodian.

These properties are exposed by implementing the corresponding interface (``Fungible`` and ``Transferable``, respectively).

.. _implementations-1:

Implementations
===============

Implementations are provided in ``Daml.Finance.Holding`` for

-  a fungible and transferable holding
-  a holding which is transferable but not fungible
-  a holding which is neither transferable nor fungible

Account
*******

Finally, account contracts are used as proof of a relationship between a
``custodian`` and an asset ``owner``.

An ``owner`` must have an account contract with a ``custodian`` before a holding
contract can be created.

This is similar to how, in the real world, you need to open a bank account
before you can use the bank’s services.

In the library, accounts are used to prevent holding transfers to
unvetted third parties: Alice can transfer a holding to Bob only
if Bob has an account at the Bank (and has therefore been vetted
by the Bank).

.. _signatories-2:

Signatories
===========

An account is co-signed by the account ``owner`` and the ``custodian``.

Keys
====

Accounts are keyed by an ``AccountKey``, which comprises

-  the account ``owner``
-  the account ``custodian``
-  a textual ``id``

.. _implementations-2:

Implementations
===============

A base account implementation is provided in ``Daml.Finance.Holding``.
